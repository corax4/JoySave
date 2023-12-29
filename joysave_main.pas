{
MIT License

Copyright (c) 2022 Yuri Lychakov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}

unit joysave_main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, base64, fpjson, types, LazUTF8,
    Spin, ExtCtrls, Grids, IniPropStorage, MaskEdit, ComCtrls, HTTPSend, synacode, dateutils, jsonparser,
    strutils, LazFileUtils, blcksock, {$IFDEF WINDOWS} Windows,{$ENDIF} ssl_openssl, zipper, ZStream, eventlog;

type

    { THTTPThread }

    THTTPThread = class(TThread)
    Private
        HTTP: THTTPSend;
        procedure HTTP_OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
        procedure ProxySettings;
    Protected
        procedure Execute; Override;
    Public
        Doc: TMemoryStream;
        Headers: TStringList;
        Go: Boolean;
        URL: String;
        referer: String;
        ProxyHost: String;
        ProxyPort: String;
        ProxyUser: String;
        ProxyPass: String;
        ProxyType: Integer;
        Cookie: String;
        PostJSON: String;
        ResultCode: Integer;
        constructor Create(CreateSuspended: Boolean);
    end;

    { TJoySaveMainForm }

    TJoySaveMainForm = class(TForm)
        btnGetHTML: TButton;
        btnSaveImg: TButton;
        btnFindImgs: TButton;
        btnFindPosts: TButton;
        btnStart: TButton;
        btnStop: TButton;
        btnIncRow: TButton;
        cbSaveWebmIfNoGif: TCheckBox;
        cbPackToCBZ: TCheckBox;
        cbDelAfterPack: TCheckBox;
        cbSaveFromComments: TCheckBox;
        cbAniGif: TCheckBox;
        cbAniWebm: TCheckBox;
        cbAniMp4: TCheckBox;
        cbSaveGifIfNoWebm: TCheckBox;
        cbSaveJpgPng: TCheckBox;
        cbAltFileName: TCheckBox;
        cbRenameToAltName: TCheckBox;
        cbSaveTagsInfo: TCheckBox;
        cbAllTagsToName: TCheckBox;
        Edit_Cookie: TEdit;
        Edit_ProxyPass: TEdit;
        Edit_proxyUser: TEdit;
        Edit_FileName: TEdit;
        Edit_proxyHost: TEdit;
        Edit_ProxyPort: TEdit;
        EventLog1: TEventLog;
        gbAnimation: TGroupBox;
        IniPropStorage1: TIniPropStorage;
        lblPHost: TLabel;
        lblTimer: TLabel;
        lblPage: TLabel;
        lblEnd: TLabel;
        lblPages: TLabel;
        lblImagesOnPage: TLabel;
        lblPPort: TLabel;
        Label20: TLabel;
        lblRow: TLabel;
        lblCookie: TLabel;
        lblPUser: TLabel;
        lblPPass: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        lblBegin: TLabel;
        MemoHelp: TMemo;
        Memo_Doc: TMemo;
        Memo_Header: TMemo;
        Memo_Imgs: TMemo;
        Memo_SG_Save: TMemo;
        Memo_Posts: TMemo;
        PageControlMain: TPageControl;
        rgProxy: TRadioGroup;
        SpinBegin: TSpinEdit;
        SpinSearchFolderRange: TSpinEdit;
        SpinRow: TSpinEdit;
        SpinTimer: TSpinEdit;
        SpinPage: TSpinEdit;
        SpinEnd: TSpinEdit;
        SG: TStringGrid;
        SpinPageCount: TSpinEdit;
        StatusBar1: TStatusBar;
        TrayIcon1: TTrayIcon;
        tsHelp: TTabSheet;
        tsDebug: TTabSheet;
        tsSettings: TTabSheet;
        tsMain: TTabSheet;
        Timer1: TTimer;
        procedure btnGetHTMLClick(Sender: TObject);
        procedure btnIncRowClick(Sender: TObject);
        procedure btnSaveImgClick(Sender: TObject);
        procedure btnFindImgsClick(Sender: TObject);
        procedure btnFindPostsClick(Sender: TObject);
        procedure btnStartClick(Sender: TObject);
        procedure btnStopClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure IniPropStorage1RestoreProperties(Sender: TObject);
        procedure IniPropStorage1RestoringProperties(Sender: TObject);
        procedure IniPropStorage1SavingProperties(Sender: TObject);
        procedure SGGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
        procedure SpinRowChange(Sender: TObject);
        procedure SpinTimerChange(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure TrayIcon1Click(Sender: TObject);
    Private
        { private declarations }
    Public
        { public declarations }
        procedure SetProxy;
        function GetImgID(Src: String; var IsComment: Boolean): String;
        function GetPostID(Src: String): String;
        procedure ZipProgress(Sender: TObject; const Pct: Double);
        procedure ZipFolder;
        procedure GetInThread(URL: String);
        procedure SaveImgProc;
        procedure GenJsonPosts;
        procedure GetLastPage;
    end;

var
    JoySaveMainForm: TJoySaveMainForm;
    Stop: Boolean = True;
    PostNow: Integer = 0;
    ImgNow: Integer = 0;
    sURL: String = '';
    MainURL: String = '';
    AppendURL: String = '';
    imgs: Integer = 0;
    Total: Integer = 0;
    StageNow: Integer = 0;
    DirNow: String = 'Pic/';
    SubDir: String;
    glbHTTP: THTTPSend = nil;
    SaveSecond: Shortint = -1;
    StartCommand: Boolean = False;
    oz: TZipper;
    HThread: THTTPThread;
    username: String = '';
    AppLoaded: Boolean = False;
    TagList: TStringList;
    IsTag: boolean;
    TagStr: string;
    TagType: string;
    Global_URL: string;
    Global_Append: string;
    NoTags: TStringDynArray;
    ExeDir: string;

implementation

{$R *.lfm}

{ THTTPThread }

procedure THTTPThread.HTTP_OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
begin
    if Stop then HTTP.Abort;
end;

procedure THTTPThread.ProxySettings;
begin
    HTTP.Headers.Append('Referer: ' + referer);
    if ProxyType = 0 then
    begin
        HTTP.Sock.SocksIP := '';
        HTTP.ProxyHost := '';
    end;
    if ProxyType = 1 then
    begin
        HTTP.ProxyHost := proxyHost;
        HTTP.ProxyPort := ProxyPort;
        HTTP.ProxyUser := proxyUser;
        HTTP.ProxyPass := ProxyPass;
        HTTP.Sock.SocksIP := '';
    end;
    HTTP.Sock.CreateWithSSL(TSSLOpenSSL);
    if ProxyType > 1 then
    begin
        HTTP.ProxyHost := '';
        HTTP.Sock.SocksIP := proxyHost;
        HTTP.Sock.SocksPort := ProxyPort;
        HTTP.Sock.SocksUsername := proxyUser;
        HTTP.Sock.SocksPassword := ProxyPass;
        HTTP.Sock.SocksResolver := True;
    end;

    if ProxyType = 2 then
        HTTP.Sock.SocksType := ST_Socks4;
    if ProxyType = 3 then
        HTTP.Sock.SocksType := ST_Socks5;
    if length(Cookie) > 5 then
        HTTP.Cookies.Text := Cookie
    else
        HTTP.Cookies.Text := '';
end;

procedure THTTPThread.Execute;
var
    ss: TStringStream = nil;
begin
    while (not Terminated) do
    begin
        if Go then
        begin
            if (Length(URL) > 0) and not stop then
            begin
                try
                    ResultCode := 0;
                    Doc.Clear;
                    Headers.Clear;
                    HTTP := THTTPSend.Create;
                    ProxySettings;
                    if Length(PostJSON) = 0 then
                        HTTP.HTTPMethod('GET', URL)
                    else
                    begin
                        HTTP.Cookies.Text := '';
                        ss := TStringStream.Create(PostJSON);
                        HTTP.MimeType := 'application/json; charset=UTF-8';
                        HTTP.Document.LoadFromStream(ss);
                        HTTP.HTTPMethod('POST', 'https://api.joyreactor.cc/graphql');
                        PostJSON := '';
                    end;
                    ResultCode := HTTP.ResultCode;
                    Headers.Assign(HTTP.Headers);
                    Doc.LoadFromStream(HTTP.Document);
                finally
                    FreeAndNil(HTTP);
                    if Assigned(ss) then FreeAndNil(ss);
                end;
            end;
            Go := False;
        end;
        sleep(10);
    end;
end;

constructor THTTPThread.Create(CreateSuspended: Boolean);
begin
    FreeOnTerminate := True;
    Go := False;
    HTTP := nil;
    Doc := TMemoryStream.Create;
    Headers := TStringList.Create;
    inherited Create(CreateSuspended);
end;

{ TJoySaveMainForm }

// загружаем HTML в Memo_Header и Memo_Doc
procedure TJoySaveMainForm.btnGetHTMLClick(Sender: TObject);
begin
    GetInThread(sURL);
    Memo_Header.Lines.Assign(HThread.Headers);
    Memo_Doc.Lines.LoadFromStream(HThread.Doc);
end;

// переход на следующую строку таблицы
procedure TJoySaveMainForm.btnIncRowClick(Sender: TObject);
var
    p: Integer;
    s: String;
    i: Integer;
begin
    PostNow := 0;
    ImgNow := 0;
    if StageNow > 99 then
    begin
        StageNow := -1;
        SpinRow.Value := -1;
        btnStopClick(nil);
        exit;
    end;
    // проверка на заполенность строки в таблице
    while not ((length(SG.Cells[1, StageNow + 1]) > 0) and (StrToIntDef(SG.Cells[2, StageNow + 1], -1) > 0) and
            (StrToIntDef(SG.Cells[3, StageNow + 1], -1) > -1)) do
    begin
        Inc(StageNow);
        SpinPage.Value := 0;
        if StageNow > 99 then Break;
    end;
    if StageNow > 99 then
    begin
        StageNow := -1;
        SpinRow.Value := -1;
        btnStopClick(nil);
    end
    else    // строка в таблице вроде в порядке
    begin
        Total := 0;
        SpinRow.Value := StageNow;
        s := SG.Cells[1, StageNow + 1]; // URL
        IsTag := LowerCase(copy(s, 1, 8)) <> 'https://';
        if IsTag then
        begin
            TagType := '';
            p := pos('/', s);
            if p <> 0 then
            begin
                TagStr := LowerCase( copy(s, 1, p - 1) );
                TagType := UpperCase( copy(s, p + 1, length(s)) );
            end
            else
                TagStr := LowerCase(s);
            if (TagType <> 'ALL') and (TagType <> 'GOOD') and (TagType <> 'NEW') and (TagType <> 'BEST') then
                    TagType := 'ALL';
            Global_Append := s;
            Global_URL := 'https://api.joyreactor.cc';
        end
        else  // it is https
        begin
            p := posex('/', s, 10); // находим ближайший '/' после https://
            if p > 1 then p := p - 1;
            if p <> 0 then Global_URL := LeftStr(s, p) else Global_URL := s;
            Global_Append := RightStr(s, length(s) - p);
            if p = 0 then Global_Append := '';
            // удаляем номер страницы, если есть
            s := Global_Append;
            p := RPos('/', s);
            if p > 4 then
            begin
                if (StrToIntDef(RightStr(s, Length(s) - p), -1) > -1) {numb} and (MidStr(s, p - 4, 5) <> '/tag/') then
                    Global_Append := LeftStr(s, p - 1);
            end;
        end;

        SpinBegin.Value := StrToIntDef(SG.Cells[2, StageNow + 1], -1);
        SpinEnd.Value := StrToIntDef(SG.Cells[3, StageNow + 1], 0);
        NoTags := SplitString(UTF8LowerCase(SG.Cells[5, StageNow + 1]), ';');
        for i := 0 to High(NoTags) do NoTags[i] := trim(NoTags[i]);

        if (SpinPage.Value < SpinBegin.Value) or (SpinPage.Value > SpinEnd.Value) then
            SpinPage.Value := SpinBegin.Value;
    end;
end;

// качаем и сохраняем картинку из sURL
procedure TJoySaveMainForm.btnSaveImgClick(Sender: TObject);
begin
    GetInThread(sURL);
    // бывает получает только 206 байт. Если меньше 350 байт, пробуем перескачать в таком случае
    if HThread.Doc.Size < 350 then
        GetInThread(sURL);
    // еще раз
    if HThread.Doc.Size < 350 then
        GetInThread(sURL);
    Memo_Header.Lines.Assign(HThread.Headers);
    if (Memo_Header.Lines.Count > 3) and (HThread.Doc.Size > 349) then
        if LeftStr(Memo_Header.Lines.Strings[3], 18) <> 'Content-Type: text' then
            try
                SaveSecond := secondof(now);
                HThread.Doc.SaveToFile(DirNow + Edit_FileName.Text);
                Inc(Total);
            except
                EventLog1.Log('Не могу сохранить файл: ' + DirNow + Edit_FileName.Text);
            end;
end;

// ищем ссылки на картинки в посте
procedure TJoySaveMainForm.btnFindImgsClick(Sender: TObject);
var
    hasVideo: Boolean;
    ImgID: String;
    Imgtype: String;

    StrSt: TStringStream;
    tags: String;
    info: String;
    rating: Single;
    PostFNum: String;

    jFull: TJSONData;
    jData: TJSONData;
    jItem: TJSONData;
    jPost: TJSONData;
    jPostAr: TJSONArray;
    jComm: TJSONArray;
    i, p: Integer;
    NodeName: String;
    Dbg: string;

    procedure ProcAttrib(Attr: TJSONArray);
    var
        j: Integer;
    begin
        if Attr <> nil then for j := 0 to Attr.Count - 1 do
            begin
                jItem := Attr.Items[j].FindPath('id');
                if jItem = nil then Continue;
                ImgID := jItem.Value;
                jItem := Attr.Items[j].FindPath('image.type');
                if jItem = nil then Continue;
                Imgtype := jItem.Value;
                jItem := Attr.Items[j].FindPath('image.hasVideo');
                if jItem = nil then Continue;
                hasVideo := jItem.Value;
                if Length(Imgtype) < 3 then Continue;
                if (Imgtype = 'GIF') or (Imgtype = 'WEBM') or (Imgtype = 'MP4') then
                begin
                    if (Imgtype = 'GIF') then
                    begin
                        if cbAniGif.Checked or (cbSaveGifIfNoWebm.Checked and (not hasVideo) and
                            (cbAniMp4.Checked or cbAniWebm.Checked)) then
                            Memo_Imgs.Append(PostFNum + 'G' + ImgID + '@' + tags);
                        if hasVideo and cbAniWebm.Checked then
                            Memo_Imgs.Append(PostFNum + 'W' + ImgID + '@' + tags);
                        if hasVideo and cbAniMp4.Checked then
                            Memo_Imgs.Append(PostFNum + 'M' + ImgID + '@' + tags);
                    end;
                    if (Imgtype = 'WEBM') and (cbAniWebm.Checked or cbSaveWebmIfNoGif.Checked) then
                    begin
                        Memo_Imgs.Append(PostFNum + 'W' + ImgID + '@' + tags);
                    end;
                    if (Imgtype = 'MP4') and (cbAniMp4.Checked or cbSaveWebmIfNoGif.Checked) then
                    begin
                        Memo_Imgs.Append(PostFNum + 'M' + ImgID + '@' + tags);
                    end;
                end
                else if cbSaveJpgPng.Checked then
                    Memo_Imgs.Append(PostFNum + LeftStr(Imgtype, 1) + ImgID + '@' + tags);
                imgs := Memo_Imgs.Lines.Count;
            end;
    end;

    procedure ProcPost(aPost: TJSONData);
    var
        k, ti, tj: Integer;
        aTags: TStringDynArray;
        bTags: TStringDynArray;
        tag: ansistring;
    begin
        // Post num
        jItem := aPost.FindPath('id');
        if jItem = nil then exit;
        PostFNum := GetPostID(jItem.Value);
        while Length(PostFNum) < 9 do PostFNum := '0' + PostFNum;
        if Length(PostFNum) > 9 then PostFNum := RightStr(PostFNum, 9);

        // tags
        jItem := aPost.FindPath('seoAttributes.title');
        if jItem = nil then exit;
        tags := jItem.Value;
        p := RPos('/ ', tags);
        if p <> 0 then tags := copy(tags, p + 2, length(tags));
        atags := SplitString(tags, ' :: ');
        btags := SplitString(UTF8lowercase(tags), ' :: ');
        for ti := 0 to High(NoTags) do
            for tj := 0 to High(bTags) do
                begin
                    tag := trim(bTags[tj]);
                    if NoTags[ti] = tag Then exit;
                end;

        jItem := aPost.FindPath('createdAt');
        if jItem = nil then exit;
        info := tags + #13#10 + 'createdAt: ' + jItem.Value;

        jItem := aPost.FindPath('nsfw');
        if jItem = nil then exit;
        info := info + #13#10 + 'nsfw: ' + BoolToStr(jItem.Value, true);

        jItem := aPost.FindPath('unsafe');
        if jItem = nil then exit;
        info := info + #13#10 + 'unsafe: ' + BoolToStr(jItem.Value, true);

        jItem := aPost.FindPath('user.username');
        if jItem = nil then exit;
        info := info + #13#10 + 'username: ' + jItem.Value;

        jItem := aPost.FindPath('rating');
        if jItem = nil then exit;
        rating := jItem.Value;
        info := info + #13#10 + 'rating: ' + FloatToStrF(rating, ffFixed, 6, 1);
        if (Length(tags) > 0) and cbSaveTagsInfo.Checked then
        begin
            StrSt := TStringStream.Create(info);
            try
                StrSt.SaveToFile(DirNow + PostFNum + '.txt');
            except
                EventLog1.Log('Не могу сохранить файл: ' + DirNow + PostFNum + '.txt');
            end;
            StrSt.Free;
        end;

        ProcAttrib(TJSONArray(aPost.FindPath('attributes')));
        // from comments
        jComm := TJSONArray(aPost.FindPath('comments'));
        if jComm = nil then exit;
        for k := 0 to jComm.Count - 1 do
            ProcAttrib(TJSONArray(jComm.Items[k].FindPath('attributes')));
    end;

begin
    if HThread.ResultCode <> 200 then exit;
    Dbg := Memo_Doc.Text;
    try
        jFull := GetJSON(Memo_Doc.Text);
    except
        jFull := GetJSON('{}');
    end;
    jData := jFull.FindPath('data');
    if jData = nil then exit;
    if IsTag then
    begin
        jPostAr := TJSONArray(jData.FindPath('tag.postPager.posts'));
        if jPostAr = nil then exit;
        for i := 0 to jPostAr.Count - 1 do
            ProcPost(jPostAr[i]);
    end
    else
    for i := 0 to jData.Count - 1 do
    begin
        // node -- post
        NodeName := TJSONObject(jData).Names[i];
        jPost := jData.FindPath(NodeName);
        if jPost = nil then Continue;
        ProcPost(jPost);
    end;
end;

// поиск постов на странице
procedure TJoySaveMainForm.btnFindPostsClick(Sender: TObject);
var
    docLen: Integer;
    s: String;
    p, p1, p2: Integer;
begin
    s := Memo_Doc.Text;
    docLen := length(s);
    p := posex('<a href="/post/', s);
    while p <> 0 do
    begin
        p1 := p + 15;
        if not (s[p1] in ['0'..'9']) then
        begin
            p := posex('<a href="/post/', s, p1);
            Continue;
        end;

        if docLen < (p1 + 20) then
            Break;

        p2 := posex('"', s, p1);
        if p2 = 0 then
            p := 0
        else
        begin
            Memo_Posts.Append(midstr(s, p1, p2 - p1));
            p := posex('<a href="/post/', s, p1);
        end;
    end;

    // username
    username := '';
    p := posex('id="settings">', s);
    if p = 0 then exit;
    p := p + Length('id="settings">');
    p1 := posex('</a>', s, p);
    if (p1 <> 0) and ((p1 - p) < 100) then
        username := '  [' + Trim(midstr(s, p, p1 - p)) + ']';
end;

// кнопка старт
procedure TJoySaveMainForm.btnStartClick(Sender: TObject);
begin
    Stop := False;
    btnStart.Enabled := False;
    ImgNow := 0;
    PostNow := 0;

    StageNow := SpinRow.Value;
    if (StageNow < 0) or (StageNow > 99) then
    begin
        StageNow := 0;
        SpinPage.Value := 0;
    end;
    btnIncRowClick(nil);
    if StageNow < 0 then exit;
    if SpinPage.Value = 0 then
        SpinPage.Value := SpinBegin.Value;
    timer1.Interval := SpinTimer.Value;

    StatusBar1.Panels.Items[0].Text := 'Кач' + username;
    SpinRow.Enabled := False;
    IniPropStorage1.Save;
    Application.Title := IntToStr(StageNow) + ':' + IntToStr(SpinPage.Value) + '/' + IntToStr(SpinEnd.Value);
    if not ForceDirectories(ExeDir + 'Pic/') then
        EventLog1.Log('Не могу создать папку: Pic');
    Timer1.Enabled := True;
end;

// кнопка стоп
procedure TJoySaveMainForm.btnStopClick(Sender: TObject);
begin
    Stop := True;
    btnStart.Enabled := True;
    Timer1.Enabled := False;
    SpinRow.Enabled := True;
    StatusBar1.Panels.Items[0].Text := 'Стоп';
    Application.Title := 'JoySave';
end;

procedure TJoySaveMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    if stop then exit;
    btnStopClick(nil);
    HThread.Terminate;
    sleep(500);
end;

procedure TJoySaveMainForm.FormCreate(Sender: TObject);
var
    rsSources: TResourceStream;
    uz: TUnZipper;
begin
    ExeDir := ExtractFilepath( Application.ExeName);
    //ShowMessage(GetUserDir());
    {$IFDEF DARWIN}
        IniPropStorage1.IniFileName := ExeDir + '../../../' + ExtractFileNameOnly(Application.ExeName) + '.ini';
        EventLog1.FileName := ExeDir + '../../../' + ExtractFileNameOnly(Application.ExeName) + '.log';
        if not ForceDirectories(ExeDir + '../../../' + 'Pic/') then
            EventLog1.Log('Не могу создать папку: Pic');
    {$ELSE}
        if not ForceDirectories(ExeDir + 'Pic/') then
            EventLog1.Log('Не могу создать папку: Pic');
    {$ENDIF}
    {$IFDEF WINDOWS}
    if not (FileExistsUTF8('libeay32.dll') and FileExistsUTF8('ssleay32.dll')) then
    begin
        rsSources := TResourceStream.Create(HINSTANCE, 'LIBS',  RT_RCDATA);
        uz := TUnZipper.Create;
        try
            rsSources.SaveToFile('libs_ssl.zip');
            uz.FileName := 'libs_ssl.zip';
            uz.Examine;
            uz.UnZipFile('libeay32.dll');
            uz.UnZipFile('ssleay32.dll');
        finally
            FreeAndNil(rsSources);
            FreeAndNil(uz);
            DeleteFileUTF8('libs_ssl.zip');
        end;
    end;
    {$ENDIF}
    if ParamCount > 0 then
        if LowerCase(ParamStr(1)) = 'sources' then
        begin
            rsSources := TResourceStream.Create(HINSTANCE, 'SOURCES', RT_RCDATA);
            try
                rsSources.SaveToFile({$IFDEF DARWIN} ExeDir + '../../../' + {$ENDIF} 'JoySave_sources.zip');
            finally
                FreeAndNil(rsSources);
            end;
            rsSources := TResourceStream.Create(HINSTANCE, 'LIBS', RT_RCDATA);
            try
                rsSources.SaveToFile('openssl-1.0.2u-i386-win32.zip');
            finally
                FreeAndNil(rsSources);
            end;
        end;
    if LowerCase(ParamStr(1)) = 'start' then StartCommand := True;
    Memo_Doc.Clear;
    Memo_Header.Clear;
    Memo_Imgs.Clear;
    Memo_Posts.Clear;
    Global_URL := '';
    Edit_FileName.Text := '';
    Edit_proxyHost.Text := '';
    Edit_ProxyPort.Text := '';
    Global_Append := '';

    HThread := THTTPThread.Create(False);
    TagList := TStringList.Create;
end;

procedure TJoySaveMainForm.IniPropStorage1RestoreProperties(Sender: TObject);
var
    i: Integer;
    RowStr: TStringList;
begin
    RowStr := TStringList.Create;
    SG.RowCount := 101;
    for i := 1 to SG.RowCount - 1 do
    begin
        RowStr.CommaText := IniPropStorage1.StoredValue['MainGrid_row_' + IntToStr(i - 1)];
        SG.Rows[i] := RowStr;
        if i <= 10 then
            SG.Cells[0, i] := '0' + IntToStr(i - 1)
        else
            SG.Cells[0, i] := IntToStr(i - 1);
    end;

    RowStr.Free;

    if StartCommand then btnStartClick(nil);
    StartCommand := False;
    AppLoaded := True;
end;

procedure TJoySaveMainForm.IniPropStorage1RestoringProperties(Sender: TObject);
var
    i: Integer;
begin
    for i := 1 to SG.RowCount - 1 do
        IniPropStorage1.StoredValue['MainGrid_row_' + IntToStr(i - 1)] := '';
end;

procedure TJoySaveMainForm.IniPropStorage1SavingProperties(Sender: TObject);
var
    i: Integer;
begin
    for i := 1 to SG.RowCount - 1 do
        IniPropStorage1.StoredValue['MainGrid_row_' + IntToStr(i - 1)] := SG.Rows[i].CommaText;
end;

procedure TJoySaveMainForm.SGGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
    case ACol of
        0: HintText := 'Номер строки, номер задания';
        1: HintText := 'Адрес - с Джоя. Например, https://joyreactor.cc/tag/котэ'#13#10 +
            'В качестве адреса можно писать тег. Например, котэ/best.'#13#10 +
            'Если поле пустое - строка пропускается.';
        2: HintText := 'Начало - первая страница тега для сохранения.'#13#10 +
            'Если поле пустое - строка пропускается.';
        3: HintText := 'Конец - последняя страница для сохранения. 0 для самой актуальной.'#13#10 +
            'Если поле пустое - строка пропускается.';
        4: HintText := 'Папка - имя папки куда сохранять. Можно с подпапками через "/".'#13#10 +
            'Если поле пустое - сохраняется в папку с номером строки.';
        5: HintText := 'Теги через ";". Если хоть один из тегов есть в посте - пост пропускается.';
    end;
end;

procedure TJoySaveMainForm.SpinRowChange(Sender: TObject);
begin
    if AppLoaded then
        SpinPage.Value := 0;
end;

procedure TJoySaveMainForm.SpinTimerChange(Sender: TObject);
begin
    Timer1.Interval := SpinTimer.Value;
end;

procedure TJoySaveMainForm.Timer1Timer(Sender: TObject);
var
    s: string;
    i: Integer;
begin
    if Stop then
        exit;
    if secondof(now) = SaveSecond then
        exit;
    Timer1.Enabled := False;

    // если картинки еще не получены
    if ImgNow = 0 then
        // если посты еще не получены
        if PostNow = 0 then
        begin
            // если конечная страница 0, то получаем номер последней страницы для тега
            if SpinEnd.Value = 0 then GetLastPage;
            s := Memo_Doc.Lines.Text;
            if copy(s, 1, Length('{"errors":[{"message":"Rate')) = '{"errors":[{"message":"Rate' then
            begin
                StatusBar1.Panels.Items[1].Text := 'Всего: ' + IntToStr(Total) + '  Lim';
                for i := 1 to 1024 do Application.ProcessMessages;
                SaveSecond := SecondOf(now);
                timer1.Enabled := True;
                exit;
            end;
            TagList.Clear;

            if SpinPage.Value > SpinEnd.Value then
            begin
                Inc(StageNow);
                SpinPage.Value := 0;
                btnIncRowClick(nil);
                if StageNow = -1 then
                begin
                    btnStopClick(nil);
                    exit;
                end;
            end;

            Memo_Imgs.Clear;
            Memo_Posts.Clear;
            if IsTag then
            begin
                Memo_Posts.Lines.Text := 'API';
            end
            else
            begin
                sURL := Global_URL + Global_Append;
                if SpinPage.Value <> 0 then
                    sURL := sURL + '/' + SpinPage.Text;
                Edit_FileName.Text := DecodeURL(sURL);
                btnGetHTMLClick(nil);
                btnFindPostsClick(nil);
            end;
            imgs := 0;
            if Memo_Posts.Lines.Count > 0 then
            begin
                PostNow := 1;
            end
            else
            begin
                SpinPage.Value := SpinPage.Value + 1;
                if ((SpinPage.Value mod SpinPageCount.Value) = 0) and cbPackToCBZ.Checked then ZipFolder;
                Application.Title := IntToStr(StageNow) + ':' + IntToStr(SpinPage.Value) + '/' + IntToStr(SpinEnd.Value);
            end;
        end
        // посты получены
        else
        begin
            sURL := 'api'; //Global_URL + Memo_Posts.Lines.Strings[PostNow - 1];
            Edit_FileName.Text := 'API';

            SubDir := ReplaceStr(Trim(SG.Cells[4, StageNow + 1]), '\', '/');
            if SubDir = '' then SubDir := Trim(SG.Cells[0, StageNow + 1]);
            DirNow := {$IFDEF DARWIN} ExeDir + '../../../' + {$ENDIF} 'Pic/' + SubDir + '/' +
                IntToStr((SpinPage.Value div SpinPageCount.Value) *
                SpinPageCount.Value) + '/';
            if not ForceDirectories(DirNow) then
                EventLog1.Log('Не могу создать папку: ' + DirNow);

            if Memo_Posts.Lines.Count > 0 then
            begin
                GenJsonPosts;
                SaveSecond := SecondOf(now);
                s := Memo_Doc.Lines.Text;
                if copy(s, 1, Length('{"errors":[{"message":"Rate')) = '{"errors":[{"message":"Rate' then
                begin
                    StatusBar1.Panels.Items[1].Text := 'Всего: ' + IntToStr(Total) + '  Lim';
                    for i := 1 to 1024 do Application.ProcessMessages;
                    SaveSecond := SecondOf(now);
                    timer1.Enabled := True;
                    exit;
                end;
                btnFindImgsClick(nil);

                if Memo_Imgs.Lines.Count > 0 then
                    ImgNow := 1;
            end;

            PostNow := 0;
            if ImgNow = 0 then
            begin
                SpinPage.Value := SpinPage.Value + 1;
                if ((SpinPage.Value mod SpinPageCount.Value) = 0) and cbPackToCBZ.Checked then ZipFolder;
                Application.Title := IntToStr(StageNow) + ':' + IntToStr(SpinPage.Value) + '/' + IntToStr(SpinEnd.Value);
            end;
        end
    else // картинки получены, ImgNow <> 0
        SaveImgProc;

    lblImagesOnPage.Caption := IntToStr(imgs);
    StatusBar1.Panels.Items[0].Text := 'Кач' + username;
    StatusBar1.Panels.Items[1].Text := 'Всего: ' + IntToStr(Total);
    StatusBar1.Panels.Items[2].Text := 'Файл: ' + IntToStr(ImgNow) + '/' + IntToStr(imgs);
    timer1.Enabled := True;
end;

procedure TJoySaveMainForm.TrayIcon1Click(Sender: TObject);
begin
    JoySaveMainForm.Visible := not JoySaveMainForm.Visible;
end;

// подготовка HTTP к работе - прокси и общие мелочи
procedure TJoySaveMainForm.SetProxy;
begin
    HThread.referer := Global_URL;
    if IsTag then HThread.referer := 'https://joyreactor.cc';
    HThread.ProxyType := rgProxy.ItemIndex;
    HThread.ProxyHost := Edit_proxyHost.Text;
    HThread.ProxyPort := Edit_ProxyPort.Text;
    HThread.ProxyUser := Edit_proxyUser.Text;
    HThread.ProxyPass := Edit_ProxyPass.Text;
    HThread.Cookie := Edit_Cookie.Text;
end;

// получаем ID картинки из её имени или URL
function TJoySaveMainForm.GetImgID(Src: String; var IsComment: Boolean): String;
var
    ln: Integer;
    deco: String;
begin
    Result := '';
    IsComment := False;
    deco := DecodeBase64(Src);
    ln := Length('PostAttributePicture:');
    if LeftStr(deco, ln) = 'PostAttributePicture:' then
        Result := RightStr(deco, Length(deco) - ln);
    if Result <> '' then exit;
    IsComment := True;
    ln := Length('CommentAttributePicture:');
    if LeftStr(deco, ln) = 'CommentAttributePicture:' then
        Result := RightStr(deco, Length(deco) - ln);
    if Result <> '' then exit;
    //EventLog1.Log('ID картинки неправильный: ' + deco);
end;

function TJoySaveMainForm.GetPostID(Src: String): String;
var
    deco: String;
    ln: Integer;
begin
    Result := '';
    deco := DecodeBase64(Src);
    ln := Length('Post:');
    if LeftStr(deco, ln) = 'Post:' then
        Result := RightStr(deco, Length(deco) - ln);
    if Result <> '' then exit;
    EventLog1.Log('Post ID неправильный: ' + deco);
end;

procedure TJoySaveMainForm.ZipProgress(Sender: TObject; const Pct: Double);
begin
    if Stop then oz.Terminate;
    Application.ProcessMessages;
end;

// Успаковка скачанной папки в CBZ
procedure TJoySaveMainForm.ZipFolder;
var
    TheFileList: TStringList;
    i: Integer;
    s: String;
begin
    if Length(DirNow) < 5 then exit;
    oz := TZipper.Create;
    oz.FileName := leftstr(DirNow, Length(DirNow) - 1) + '.cbz';
    TheFileList := TStringList.Create;
    oz.OnProgress := @ZipProgress;
    try
        FindAllFiles(TheFileList, DirNow);
        oz.UseLanguageEncoding := True;
        oz.Entries.AddFileEntries(TheFileList);
        for i := 0 to oz.Entries.Count - 1 do
        begin
            oz.Entries.Entries[i].CompressionLevel := clnone;
            s := oz.Entries.Entries[i].DiskFileName;
            oz.Entries.Entries[i].ArchiveFileName := RightStr(s, length(s) - 5 - length(SubDir));
        end;
        StatusBar1.Panels.Items[0].Text := 'Пак';
        Application.ProcessMessages;
        oz.ZipAllFiles;
        if cbDelAfterPack.Checked and not Stop then DeleteDirectory(DirNow, False);
        StatusBar1.Panels.Items[0].Text := 'Кач' + username;
    finally
        TheFileList.Free;
        oz.Free;
    end;
end;

procedure TJoySaveMainForm.GetInThread(URL: String);
begin
    SetProxy;
    HThread.URL := URL;
    HThread.Go := True;
    while HThread.Go do
    begin
        Application.ProcessMessages;
        sleep(1);
    end;
    HThread.URL := '';
end;

procedure TJoySaveMainForm.SaveImgProc;
var
    filename, SrvFName: String;
    p: SizeInt;
    i: integer;
    ImgFromComment: Boolean;
    s1: String;
    BadFileName: String;
    PostNum: String;
    ImgType: String;
    ImgId: String;
    Tags: String;
    aTags: TStringDynArray;
    ext: String;
    saved: boolean;

    OldFiles: TStringList;
begin
    s1 := Memo_Imgs.Lines.Strings[ImgNow - 1];
    PostNum := leftstr(s1, 9);
    ImgType := copy(s1, 10, 1);
    ImgFromComment := False;
    p := pos('@', s1);
    ImgId := GetImgID(copy(s1, 11, p - 11), ImgFromComment);
    tags := copy(s1, p + 1, length(s1));

    case ImgType of
        'G': ext := '.gif';
        'J': ext := '.jpeg';
        'P': ext := '.png';
        'B': ext := '.bmp';
        'T': ext := '.tiff';
        'W': ext := '.webm';
        'M': ext := '.mp4';
        else
            ext := '.jpeg';
    end;
    case ImgType of
        'G': if ImgFromComment then
                sURL := 'https://img10.reactor.cc/pics/comment/post-' + ImgId + ext
            else
                sURL := 'https://img10.reactor.cc/pics/post/post-' + ImgId + ext;
        'W': if ImgFromComment then
                sURL := 'https://img10.reactor.cc/pics/comment/webm/post-' + ImgId + ext
            else
                sURL := 'https://img10.reactor.cc/pics/post/webm/post-' + ImgId + ext;
        'M': if ImgFromComment then
                sURL := 'https://img10.reactor.cc/pics/comment/mp4/post-' + ImgId + ext
            else
                sURL := 'https://img10.reactor.cc/pics/post/mp4/post-' + ImgId + ext;
        else
            if ImgFromComment then
                sURL := 'https://img10.reactor.cc/pics/comment/full/post-' + ImgId + ext
            else
                sURL := 'https://img10.reactor.cc/pics/post/full/post-' + ImgId + ext;
    end;

    atags := SplitString(tags, ' :: ');

    SrvFName := DecodeURL(sURL);
    if not cbAltFileName.Checked then
        filename := SrvFName
    else
    begin
        BadFileName := '';
        OldFiles := FindAllFiles(DirNow, '*-' + ImgId + ext, False);
        //OldFiles := FindAllFiles('Pic/' + SubDir, '*-' + ImgId + '.*', true);
        if OldFiles.Count = 0 then
            for i := 1 to SpinSearchFolderRange.Value do
            begin
                if OldFiles.Count > 0 then Break;
                OldFiles.Free;
                OldFiles := FindAllFiles({$IFDEF DARWIN} ExeDir + '../../../' + {$ENDIF} 'Pic/' + SubDir + '/'
                    + IntToStr((SpinPage.Value div SpinPageCount.Value - i) *
                    SpinPageCount.Value) + '/', '*-' + ImgId + ext, False);
                if OldFiles.Count > 0 then Break;
                OldFiles.Free;
                OldFiles := FindAllFiles({$IFDEF DARWIN} ExeDir + '../../../' + {$ENDIF} 'Pic/' + SubDir + '/'
                    + IntToStr((SpinPage.Value div SpinPageCount.Value + i) *
                    SpinPageCount.Value) + '/', '*-' + ImgId + ext, False);
            end;

        if OldFiles.Count = 1 then
            BadFileName := OldFiles.Strings[0];
        if OldFiles.Count > 1 then
            EventLog1.Log('Many old files: ' + OldFiles.Text);
        OldFiles.Free;

        filename := PostNum + IfThen(ImgFromComment, '_1_', '_0_');
        while length(ImgId) < 9 do
            ImgId := '0' + ImgId;
        filename := filename + ImgId + '__';
        if not cbAllTagsToName.Checked then
        begin
            if length(aTags) > 0 then filename := filename + ReplaceStr(aTags[0], ' ', '-');
            if length(aTags) > 1 then filename := filename + '-' + ReplaceStr(aTags[1], ' ', '-');
            if length(aTags) > 2 then filename := filename + '-' + ReplaceStr(aTags[2], ' ', '-');
            if length(aTags) > 3 then filename := filename + '-' + ReplaceStr(aTags[3], ' ', '-');
        end
        else
        begin
            filename := filename + ReplaceStr(Tags, ' :: ', '=');
        end;
        filename := filename + ext;
    end;
    filename := ReplaceStr(filename, '\', '@');
    filename := ReplaceStr(filename, '/', '@');
    filename := ReplaceStr(filename, ':', '@');
    filename := ReplaceStr(filename, '*', '@');
    filename := ReplaceStr(filename, '?', '@');
    filename := ReplaceStr(filename, '|', '@');
    filename := ReplaceStr(filename, '<', '@');
    filename := ReplaceStr(filename, '>', '@');
    filename := ReplaceStr(filename, '"', '@');
    Edit_FileName.Text := filename;

    if cbAltFileName.Checked and cbRenameToAltName.Checked then
    begin
        //if FileExistsUTF8(DirNow + SrvFName) then
        //    RenameFileUTF8(DirNow + SrvFName, DirNow + Edit_FileName.Text);
        if (Length(BadFileName) > 0) and FileExistsUTF8(BadFileName) then
            RenameFileUTF8(BadFileName, DirNow + Edit_FileName.Text);

        BadFileName := '';
        OldFiles := FindAllFiles(DirNow, '*_' + ImgId + '__*' + ext, False);
        //OldFiles := FindAllFiles('Pic/' + SubDir, '*_' + ImgId + '__*', true);
        if OldFiles.Count = 0 then
            for i := 1 to SpinSearchFolderRange.Value do
            begin
                if OldFiles.Count > 0 then Break;
                OldFiles.Free;
                OldFiles := FindAllFiles({$IFDEF DARWIN} ExeDir + '../../../' + {$ENDIF} 'Pic/' + SubDir + '/'
                    + IntToStr((SpinPage.Value div SpinPageCount.Value - i) *
                    SpinPageCount.Value) + '/', '*_' + ImgId + '__*' + ext, False);
                if OldFiles.Count > 0 then Break;
                OldFiles.Free;
                OldFiles := FindAllFiles({$IFDEF DARWIN} ExeDir + '../../../' + {$ENDIF} 'Pic/' + SubDir + '/'
                    + IntToStr((SpinPage.Value div SpinPageCount.Value + i) *
                    SpinPageCount.Value) + '/', '*_' + ImgId + '__*' + ext, False);
            end;

        if OldFiles.Count = 1 then
            BadFileName := OldFiles.Strings[0];
        if OldFiles.Count > 1 then
            EventLog1.Log('Many old files: ' + OldFiles.Text);
        OldFiles.Free;
        if BadFileName = DirNow + filename then BadFileName := '';
        if (Length(BadFileName) > 0) and FileExistsUTF8(BadFileName) then
            RenameFileUTF8(BadFileName, DirNow + Edit_FileName.Text);
    end;
    saved := false;
    if not (FileExistsUTF8(DirNow + Edit_FileName.Text) or FileExistsUTF8(DirNow + SrvFName)) then
    begin
        btnSaveImgClick(nil);
        saved := true;
    end;

    Inc(ImgNow);
    if ImgNow > Memo_Imgs.Lines.Count then
        ImgNow := 0;
    if ImgNow = 0 then
    begin
        SpinPage.Value := SpinPage.Value + 1;
        if ((SpinPage.Value mod SpinPageCount.Value) = 0) and cbPackToCBZ.Checked then ZipFolder;
        Application.Title := IntToStr(StageNow) + ':' + IntToStr(SpinPage.Value) + '/' + IntToStr(SpinEnd.Value);
    end;
    Application.ProcessMessages;
    if (ImgNow <> 0) and not saved then SaveImgProc;
end;

procedure TJoySaveMainForm.GenJsonPosts;
var
    json: String;
    i: Integer;
    PostStr: String;
begin
    json := '{"query":"{';
    if IsTag then
    begin
        if cbSaveFromComments.Checked then
            json := json + 'tag(name:\"' + TagStr + '\") { postPager(type:' + TagType +
                ' ){ posts(page:' + SpinPage.Text + ') {... on Post {id, rating, createdAt, nsfw, unsafe, ' +
                'user{username}, seoAttributes{title}, attributes{ ' +
                '... on AttributePicture {id, image { type, width, height, hasVideo }} }  comments { attributes { ' +
                '... on AttributePicture {id, image { type, width, height, hasVideo }} } } } } } }'
        else
            json := json + 'tag(name:\"' + TagStr + '\") { postPager(type:' + TagType +
                ' ){ posts(page:' + SpinPage.Text + ') {... on Post {id, rating, createdAt, nsfw, unsafe, ' +
                'user{username}, seoAttributes{title}, attributes{ ' +
                '... on AttributePicture {id, image { type, width, height, hasVideo }} } } } } }';
    end
    else if cbSaveFromComments.Checked then
        for i := 0 to Memo_Posts.Lines.Count - 1 do
        begin
            if i <> 0 then json := json + ', ';
            json := json + 'node' + IntToStr(i + 10) + ':node(id : \"';
            PostStr := EncodeBase64('Post:' + Memo_Posts.Lines.Strings[i]);
            json := json + PostStr + '\") {... on Post { id, rating, createdAt, nsfw, unsafe, ' +
                'user{username}, seoAttributes{title}, attributes{ ' +
                '... on AttributePicture {id, image { type, width, height, hasVideo }} }  comments { attributes { ' +
                '... on AttributePicture {id, image { type, width, height, hasVideo }} } } } }';
        end
    else
        for i := 0 to Memo_Posts.Lines.Count - 1 do
        begin
            if i <> 0 then json := json + ', ';
            json := json + 'node' + IntToStr(i + 10) + ':node(id : \"';
            PostStr := EncodeBase64('Post:' + Memo_Posts.Lines.Strings[i]);
            json := json + PostStr + '\") {... on Post { id, rating, createdAt, nsfw, unsafe, ' +
                'user{username}, seoAttributes{title}, attributes{ ' +
                '... on AttributePicture {id, image { type, width, height, hasVideo }} } } }';
        end;
    json := json + '}"}';
    HThread.PostJSON := json;
    GetInThread('api');
    Memo_Header.Lines.Text := HThread.Headers.Text;
    Memo_Doc.Lines.LoadFromStream(HThread.Doc);
end;

procedure TJoySaveMainForm.GetLastPage;
var
    s: TCaption;
    p, pEnd: SizeInt;
    json: string;
    last: integer;
begin
    if IsTag then
    begin
        json := '{"query":"{tag(name:\"' + TagStr + '\") { postPager(type:' + TagType + ') {count} } }"}';
        HThread.PostJSON := json;
        GetInThread('api');
        Memo_Header.Lines.Text := HThread.Headers.Text;
        Memo_Doc.Lines.LoadFromStream(HThread.Doc);
        if HThread.ResultCode <> 200 then exit;
        s := Memo_Doc.Text;
        p := PosEx('"count":', s);
        if p <> 0 then
        begin
            p := p + Length('"count":');
            pEnd := PosEx('}', s, p);
            if pEnd <> 0 then
                if StrToIntDef(MidStr(s, p, pEnd - p), -1) > 0 then
                begin
                    last := StrToIntDef(MidStr(s, p, pEnd - p), -1);
                    if last < 0 then exit;
                    if last mod 10 = 0 then
                        last := last div 10
                    else
                        last := (last div 10) + 1;
                    SG.Cells[3, StageNow + 1] := IntToStr(last);
                    SpinEnd.Value := last;
                    if SpinEnd.Value > 0 then
                        IniPropStorage1.Save;
                end;
        end;
    end
    else
    begin
        sURL := Global_URL + Global_Append;
        Edit_FileName.Text := DecodeURL(sURL);
        btnGetHTMLClick(nil);
        s := Memo_Doc.Text;
        p := PosEx('"pagination_expanded"><span class=''current''>', s);
        if p <> 0 then
        begin
            p := p + Length('"pagination_expanded"><span class=''current''>');
            pEnd := PosEx('<', s, p);
            if pEnd <> 0 then
                if StrToIntDef(MidStr(s, p, pEnd - p), -1) > 0 then
                begin
                    SG.Cells[3, StageNow + 1] := MidStr(s, p, pEnd - p);
                    SpinEnd.Value := StrToIntDef(SG.Cells[3, StageNow + 1], -1);
                    if SpinEnd.Value > 0 then
                        IniPropStorage1.Save;
                end;
        end;
    end;
end;

end.
