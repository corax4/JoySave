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
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Spin, ExtCtrls, Grids, IniPropStorage, MaskEdit, ComCtrls, HTTPSend, synacode, dateutils,
    strutils, LazFileUtils, blcksock, {$IFDEF WINDOWS} Windows,{$ENDIF} ssl_openssl, zipper, ZStream;

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
        referer: string;
        ProxyHost: string;
        ProxyPort: string;
        ProxyUser: string;
        ProxyPass: string;
        ProxyType: integer;
        Cookie: string;
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
        btnFindGifs: TButton;
        btnIncRow: TButton;
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
        Edit_URL: TEdit;
        Edit_FileName: TEdit;
        Edit_proxyHost: TEdit;
        Edit_ProxyPort: TEdit;
        Edit_append: TEdit;
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
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
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
        SpinRow: TSpinEdit;
        SpinTimer: TSpinEdit;
        SpinPage: TSpinEdit;
        SpinEnd: TSpinEdit;
        SG: TStringGrid;
        SpinPageCount: TSpinEdit;
        StatusBar1: TStatusBar;
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
        procedure btnFindGifsClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure IniPropStorage1RestoreProperties(Sender: TObject);
        procedure IniPropStorage1RestoringProperties(Sender: TObject);
        procedure IniPropStorage1SavingProperties(Sender: TObject);
        procedure SpinRowChange(Sender: TObject);
        procedure SpinTimerChange(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
    Private
        { private declarations }
    Public
        { public declarations }
        procedure SetProxy;
        function GetImgID(str: String): String;
        procedure ZipProgress(Sender: TObject; const Pct: Double);
        procedure ZipFolder;
        procedure GetInThread(URL: String);
        function GetPostNum: Cardinal;
        procedure SaveImgProc;
        procedure SaveTags;
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
        HTTP.Cookies.Text := 'joyreactor_sess3=' + Cookie
    else
        HTTP.Cookies.Text := '';
end;

procedure THTTPThread.Execute;
begin
    while (not Terminated) do
    begin
        if Go then
        begin
            if (Length(URL) > 0) and not stop then
            begin
                try
                    Doc.Clear;
                    Headers.Clear;
                    HTTP := THTTPSend.Create;
                    ProxySettings;
                    HTTP.HTTPMethod('GET', URL);
                    Headers.Assign(HTTP.Headers);
                    Doc.LoadFromStream(HTTP.Document);
                finally
                    FreeAndNil(HTTP);
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
begin
    PostNow := 0;
    ImgNow := 0;
    // проверка на заполенность строки в таблице
    while not ((length(SG.Cells[1, StageNow + 1]) > 10) and (StrToIntDef(SG.Cells[2, StageNow + 1], -1) > 0) and
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
        SpinRow.Value := StageNow;
        s := SG.Cells[1, StageNow + 1]; // URL
        p := posex('/', s, 10); // находим ближайший '/' после https://
        if p > 1 then p := p - 1;
        Edit_URL.Text := ReplaceStr(LeftStr(s, p), '//old.', '//');
        if p = 0 then Edit_URL.Text := ReplaceStr(s, '//old.', '//'); // главноая страница
        Edit_append.Text := RightStr(s, length(s) - p);
        if p = 0 then Edit_append.Text := '';
        // удаляем номер страницы, если есть
        s := Edit_append.Text;
        p := RPos('/', s);
        if p > 4 then
        begin
            if (StrToIntDef(RightStr(s, Length(s) - p), -1) > -1) {numb} and (MidStr(s, p - 4, 5) <> '/tag/') then
                Edit_append.Text := LeftStr(s, p - 1);
        end
        else
            Edit_append.Text := '';
        SpinBegin.Value := StrToIntDef(SG.Cells[2, StageNow + 1], -1);
        SpinEnd.Value := StrToIntDef(SG.Cells[3, StageNow + 1], 0);

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
                ShowMessage('Не могу сохранить файл: ' + DirNow + Edit_FileName.Text);
            end;
end;

// ищем ссылки на картинки в посте
procedure TJoySaveMainForm.btnFindImgsClick(Sender: TObject);
var
    docLen: Integer;
    s: String;
    ImgStr: String;
    p, p1, p2: Integer;
begin
    if not cbSaveJpgPng.Checked then exit;
    s := Memo_Doc.Text;
    docLen := length(s);
    p := posex('<div class="image">', s);
    if p <> 0 then
    begin
        p1 := p + Length('<div class="image">');
        p := posex('<', s, p1);
    end;
    while (p <> 0) do
    begin
        if docLen < (p + 20) then
            Break;
        // смотрим, не ссылка ли это на большую картинку
        if MidStr(s, p, Length('<a href="')) = '<a href="' then
        begin
            p1 := p + Length('<a href="');
            p2 := posex('"', s, p1);
            if p2 = 0 then
                p := 0
            else
            begin
                ImgStr := HexStr(GetPostNum, 8);
                ImgStr := ImgStr + midstr(s, p1, p2 - p1);
                Memo_Imgs.Append(ImgStr);
                Inc(imgs);
                p := posex('<div class="image">', s, p1);
                if p <> 0 then
                begin
                    p1 := p + Length('<div class="image">');
                    p := posex('<', s, p1);
                end;
            end;
        end
        // или это картинка
        else if MidStr(s, p, Length('<img src="')) = '<img src="' then
        begin
            p1 := p + Length('<img src="');
            p2 := posex('"', s, p1);
            if p2 = 0 then
                p := 0
            else
            begin
                ImgStr := HexStr(GetPostNum, 8);
                ImgStr := ImgStr + midstr(s, p1, p2 - p1);
                Memo_Imgs.Append(ImgStr);
                Inc(imgs);
                p := posex('<div class="image">', s, p1);
                if p <> 0 then
                begin
                    p1 := p + Length('<div class="image">');
                    p := posex('<', s, p1);
                end;
            end;
        end
        else
        begin
            p := posex('<div class="image">', s, p1);
            if p <> 0 then
            begin
                p1 := p + Length('<div class="image">');
                p := posex('<', s, p1);
            end;
        end;
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
            Memo_Posts.Append('/post/' + midstr(s, p1, p2 - p1));
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
    if not ForceDirectories('Pic/') then
        ShowMessage('Не могу создать папку: Pic');
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

// найти ссылки на гифки в посте
procedure TJoySaveMainForm.btnFindGifsClick(Sender: TObject);
var
    docLen: Integer;
    s: String;
    i: Integer;
    p, p1, p2: Integer;
    ids: TStringList;
    id: String;
    ImgStr: String;
    NeedGif: Boolean;
begin
    if not (cbAniGif.Checked or cbAniWebm.Checked or cbAniMp4.Checked) then
        exit;

    ids := TStringList.Create;
    s := Memo_Doc.Text;
    docLen := length(s);

    // webm
    if cbAniWebm.Checked then
    begin
        p := posex('.webm"', s);
        while p <> 0 do
        begin
            p1 := RPosEx('"', s, p);
            if (p1 <> 0) then
            begin
                ImgStr := HexStr(GetPostNum, 8);
                ImgStr := ImgStr + midstr(s, p1 + 1, p - p1 + 4 {webm});
                Memo_Imgs.Append(ImgStr);
                Inc(imgs);
                id := GetImgID(ImgStr);
                if (id <> '') then ids.Append(id);
            end;
            p := posex('.webm"', s, p + 4);
        end;
    end;

    // mp4
    if cbAniMp4.Checked then
    begin
        p := posex('.mp4"', s);
        while p <> 0 do
        begin
            p1 := RPosEx('"', s, p);
            if (p1 <> 0) then
            begin
                ImgStr := HexStr(GetPostNum, 8);
                ImgStr := ImgStr + midstr(s, p1 + 1, p - p1 + 3 {mp4});
                Memo_Imgs.Append(ImgStr);
                Inc(imgs);
                id := GetImgID(ImgStr);
                if (id <> '') then ids.Append(id);
            end;
            p := posex('.mp4"', s, p + 4);
        end;
    end;

    // gif
    p := posex('<span class="video_gif_holder">', s);
    if p <> 0 then
    begin
        p1 := p + Length('<span class="video_gif_holder">');
        p := posex('<', s, p1);
    end;
    while (p <> 0) do
    begin
        if docLen < (p + 20) then
            Break;
        // смотрим, не ссылка ли это на большую картинку
        if MidStr(s, p, Length('<a href="')) = '<a href="' then
        begin
            p1 := p + Length('<a href="');
            p2 := posex('"', s, p1);
            if p2 = 0 then
                p := 0
            else
            begin
                ImgStr := HexStr(GetPostNum, 8);
                ImgStr := ImgStr + midstr(s, p1, p2 - p1);
                NeedGif := True;
                if not cbAniGif.Checked then
                begin
                    if not cbSaveGifIfNoWebm.Checked then NeedGif := False;
                    id := GetImgID(ImgStr);
                    if (id <> '') then
                        for i := 0 to ids.Count - 1 do
                            if ids.Strings[i] = id then NeedGif := False;
                end;
                if NeedGif then
                begin
                    Memo_Imgs.Append(ImgStr);
                    Inc(imgs);
                end;
            end;
        end;
        if p <> 0 then
        begin
            p := posex('<span class="video_gif_holder">', s, p1);
            if p <> 0 then
            begin
                p1 := p + Length('<span class="video_gif_holder">');
                p := posex('<', s, p1);
            end;
        end;
    end;

    ids.Free;
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
    RowStr: TStringList;
begin
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
                rsSources.SaveToFile('JoySave_sources.zip');
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
    Edit_URL.Text := '';
    Edit_FileName.Text := '';
    Edit_proxyHost.Text := '';
    Edit_ProxyPort.Text := '';
    Edit_append.Text := '';


    // старые файлы настроек
    RowStr := TStringList.Create;
    if FileExistsUTF8('JoySave.ini') then
        try
            RowStr.LoadFromFile('JoySave.ini');
            if RowStr.Strings[0] = '[TApplication.Form1]' then
            begin
                RowStr.Strings[0] := '[TApplication.JoySaveMainForm]';
                RowStr.SaveToFile('JoySave.ini');
            end;
        finally;
            DeleteFileUTF8('cookies.txt');
        end;
    RowStr.Free;
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
    end;

    // старые файлы настроек
    RowStr.Clear;
    if FileExistsUTF8('cookies.txt') then
        try
            RowStr.LoadFromFile('cookies.txt');
            for i := 0 to RowStr.Count - 1 do
            begin
                if PosEx('joyreactor_sess3=', RowStr.Strings[i]) > 0 then
                begin
                    Edit_Cookie.Text := RightStr(RowStr.Strings[i], length(RowStr.Strings[i]) - length('joyreactor_sess3='));
                    IniPropStorage1.Save;
                    Break;
                end;
            end;
        finally;
            DeleteFileUTF8('cookies.txt');
        end;
    RowStr.Clear;

    if FileExistsUTF8('list.csv') then
        try
            SG.LoadFromCSVFile('list.csv', ';');
            SG.RowCount := 101;
            for i := 1 to 10 do
                SG.Cells[0, i] := '0' + IntToStr(i - 1);
            for i := 11 to SG.RowCount - 1 do
                SG.Cells[0, i] := IntToStr(i - 1);
            SG.Cells[0, 0] := '№';
            SG.Cells[1, 0] := 'Адрес';
            SG.Cells[2, 0] := 'Начало';
            SG.Cells[3, 0] := 'Конец';
            SG.Cells[4, 0] := 'Папка';
            IniPropStorage1.Save;
        finally;
            DeleteFileUTF8('list.csv');
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
    s: TCaption;
    p, pEnd: SizeInt;
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
            if SpinEnd.Value = 0 then
            begin
                sURL := Edit_URL.Text + Edit_append.Text;
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
            sURL := Edit_URL.Text + Edit_append.Text;
            if SpinPage.Value <> 0 then
                sURL := sURL + '/' + SpinPage.Text;
            Edit_FileName.Text := DecodeURL(sURL);
            btnGetHTMLClick(nil);
            Memo_Posts.Clear;
            Memo_Imgs.Clear;
            btnFindPostsClick(nil);
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
            sURL := Edit_URL.Text + Memo_Posts.Lines.Strings[PostNow - 1];
            Inc(PostNow);
            Edit_FileName.Text := DecodeURL(sURL);
            btnGetHTMLClick(nil);

            if PostNow = 2 then
            begin
                SubDir := ReplaceStr(Trim(SG.Cells[4, StageNow + 1]), '\', '/');
                if SubDir = '' then SubDir := Trim(SG.Cells[0, StageNow + 1]);
                DirNow := 'Pic/' + SubDir + '/' + IntToStr((SpinPage.Value div SpinPageCount.Value) *
                    SpinPageCount.Value) + '/';
                if not ForceDirectories(DirNow) then
                    ShowMessage('Не могу создать папку: ' + DirNow);
            end;
            SaveTags;
            btnFindImgsClick(nil);
            btnFindGifsClick(nil);
            if PostNow > Memo_Posts.Lines.Count then
            begin
                PostNow := 0;
                if Memo_Imgs.Lines.Count > 0 then
                    ImgNow := 1;

                if ImgNow = 0 then
                begin
                    SpinPage.Value := SpinPage.Value + 1;
                    if ((SpinPage.Value mod SpinPageCount.Value) = 0) and cbPackToCBZ.Checked then ZipFolder;
                    Application.Title := IntToStr(StageNow) + ':' + IntToStr(SpinPage.Value) + '/' + IntToStr(SpinEnd.Value);
                end;
            end;
        end
    else // картинки получены, ImgNow <> 0
        SaveImgProc;

    lblImagesOnPage.Caption := IntToStr(imgs);
    StatusBar1.Panels.Items[0].Text := 'Кач' + username;
    StatusBar1.Panels.Items[1].Text := 'Всего: ' + IntToStr(Total);
    StatusBar1.Panels.Items[2].Text := 'Пост: ' + IntToStr(PostNow);
    StatusBar1.Panels.Items[3].Text := 'Файл: ' + IntToStr(ImgNow) + '/' + IntToStr(imgs);
    timer1.Enabled := True;
end;

// подготовка HTTP к работе - прокси и общие мелочи
procedure TJoySaveMainForm.SetProxy;
begin
    HThread.referer := Edit_URL.Text;
    HThread.ProxyType := rgProxy.ItemIndex;
    HThread.ProxyHost := Edit_proxyHost.Text;
    HThread.ProxyPort := Edit_ProxyPort.Text;
    HThread.ProxyUser := Edit_proxyUser.Text;
    HThread.ProxyPass := Edit_ProxyPass.Text;
    HThread.Cookie := Edit_Cookie.Text;
end;

// получаем ID картинки из её имени или URL
function TJoySaveMainForm.GetImgID(str: String): String;
var
    pStart, pEnd: Integer;
begin
    Result := '';
    pEnd := RPos('.', str);
    if pEnd < 3 then exit;
    pStart := RPos('-', str);
    if (pStart <> 0) and (pEnd > pStart) then
        Result := MidStr(str, pStart + 1, pEnd - pStart - 1);
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

function TJoySaveMainForm.GetPostNum: Cardinal;
var
    pStart: Integer;
    s: String;
begin
    Result := 0;
    pStart := posex('/post/', sURL);
    if pStart = 0 then exit;
    pStart := pStart + Length('/post/');
    s := RightStr(sURL, Length(sURL) - pStart + 1);
    Result := StrToIntDef(s, 0);
end;

procedure TJoySaveMainForm.SaveImgProc;
var
    filename, SrvFName, ids: String;
    p: SizeInt;
    ImgFromComment: Boolean;
    i: Integer;
    s1, s2: string;
    BadFileName: string;
    BadPostNum: string;
    PostNum: string;
begin
    sURL := Memo_Imgs.Lines.Strings[ImgNow - 1];
    sURL := RightStr(sURL, Length(sURL) - 8);
    ImgFromComment := (posex('/pics/comment/', sURL) <> 0);

    BadPostNum := '';
    if cbSaveFromComments.Checked or not ImgFromComment then
    begin
        if length(sURL) > 2 then
            if (sURL[1] = '/') and (sURL[2] = '/') then sURL := 'https:' + sURL;
        SrvFName := DecodeURL(sURL);
        p := RPos('/', SrvFName);
        SrvFName := RightStr(SrvFName, Length(SrvFName) - p);
        if not cbAltFileName.Checked then
            filename := SrvFName
        else
        begin
            PostNum := LeftStr(Memo_Imgs.Lines.Strings[ImgNow - 1], 8);
            try
                PostNum := IntToStr(Hex2Dec(PostNum));
                BadPostNum := PostNum;
                while length(PostNum) < 8 do
                    PostNum := '0' + PostNum;
            except
                PostNum := '';
            end;
            ids := GetImgID(SrvFName);
            if length(BadPostNum) > 0 then
            begin
                BadPostNum := RightStr(BadPostNum, Length(BadPostNum) - 1);
                while length(BadPostNum) < 8 do
                    BadPostNum := '0' + BadPostNum;
            end;

            filename := PostNum + IfThen(ImgFromComment, '_1_', '_0_');
            while length(ids) < 9 do
                ids := '0' + ids;
            if not cbAllTagsToName.Checked then
                filename := filename + ids + '__' + ReplaceStr(SrvFName, '-' + GetImgID(SrvFName) + '.', '.')
            else
            begin
                filename := filename + ids + '__';
                p := -1;
                s1 := LeftStr(Memo_Imgs.Lines.Strings[ImgNow - 1], 8);
                for i := 0 to TagList.Count - 1 do
                begin
                    s2 := LeftStr(TagList.Strings[i], 8);
                    if s1 = s2 then
                    begin
                        p := i;
                        Break;
                    end;
                end;
                if p > -1 then
                begin
                    s2 := RightStr(TagList.Strings[p], Length(TagList.Strings[p]) - 8);
                    s2 := ReplaceStr(s2, ' :: ', '=');

                    p := RPos('.', SrvFName);
                    filename := filename + s2 + RightStr(SrvFName, Length(SrvFName) - p + 1);
                end;
            end;
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
        if BadPostNum <> '' then BadFileName := ReplaceStr(filename, PostNum + '_', BadPostNum + '_');

        if cbAltFileName.Checked and cbRenameToAltName.Checked then
        begin
            if FileExistsUTF8(DirNow + SrvFName) then
                RenameFileUTF8(DirNow + SrvFName, DirNow + Edit_FileName.Text);
            if FileExistsUTF8(DirNow + BadFileName) then
                RenameFileUTF8(DirNow + BadFileName, DirNow + Edit_FileName.Text);
        end;
        if not (FileExistsUTF8(DirNow + Edit_FileName.Text) or FileExistsUTF8(DirNow + SrvFName)) then
            btnSaveImgClick(nil);
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
end;

procedure TJoySaveMainForm.SaveTags;
var
    s: String;
    TagFileName: String;
    p, p1, p2: Integer;
    StrSt: TStringStream;
    tags: String;
begin

    s := Memo_Doc.Text;
    tags := '';
    p := posex('class="taglist">', s);
    p2 := posex('class="post_content">', s);
    if (p <> 0) and (p2 <> 0) and (p2 > p) then
    begin
        p := p + Length('class="taglist">');
        p2 := RPosEx('</a>', s, p2);
        p1 := RPosEx('>', s, p2);
        while p1 > p do
        begin
            p1 := p1 + 1;
            if tags = '' then
                tags := MidStr(s, p1, p2 - p1)
            else
                tags := tags + ' :: ' + MidStr(s, p1, p2 - p1);
            p2 := RPosEx('</a>', s, p1);
            p1 := RPosEx('>', s, p2);
        end;
    end;
    if tags <> '' then
    begin
        TagList.Append(HexStr(GetPostNum, 8) + tags);
        tags := tags + LineEnding;
        StrSt := TStringStream.Create(tags);
        TagFileName := IntToStr(GetPostNum);
        while length(TagFileName) < 8 do
            TagFileName := '0' + TagFileName;
        if cbSaveTagsInfo.Checked then
            StrSt.SaveToFile(DirNow + TagFileName + '.txt');
        StrSt.Free;
    end;
end;

end.
