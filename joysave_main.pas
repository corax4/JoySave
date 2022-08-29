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

    { TForm1 }

    TForm1 = class(TForm)
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
		Edit_ProxyPass: TEdit;
		Edit_proxyUser: TEdit;
        Edit_URL: TEdit;
        Edit_FileName: TEdit;
        Edit_proxyHost: TEdit;
        Edit_ProxyPort: TEdit;
        Edit_append: TEdit;
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
        Label22: TLabel;
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
        Memo_Cookies: TMemo;
        Memo_Imgs: TMemo;
        Memo_Posts: TMemo;
        PageControl1: TPageControl;
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
        procedure SpinTimerChange(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
    private
        { private declarations }
        procedure HTTP_OnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    public
        { public declarations }
        procedure SetProxy;
    end;

var
    Form1: TForm1;
    Stop: boolean = True;
    PostNow: integer = 0;
    ImgNow: integer = 0;
    sURL: string = '';
    MainURL: string = '';
    AppendURL: string = '';
    imgs: integer = 0;
    Total: integer = 0;
    StageNow: integer = 0;
    DirNow: String = 'Pic/';
    SubDir: string;
    glbHTTP: THTTPSend = nil;
    SaveSecond: ShortInt = -1;
    PrevDir: string = '';

implementation

{$R *.lfm}

{ TForm1 }

// загружаем HTML в Memo_Header и Memo_Doc
procedure TForm1.btnGetHTMLClick(Sender: TObject);
var
    HTTP: THTTPSend;
begin
    HTTP := THTTPSend.Create;
    glbHTTP := HTTP;
    try
        SetProxy;
        HTTP.Cookies.Text := Memo_Cookies.Lines.Text;
        HTTP.Sock.OnStatus := @HTTP_OnStatus;
        HTTP.HTTPMethod('GET', sURL);

        Memo_Header.Lines.Assign(HTTP.Headers);
        Memo_Doc.Lines.LoadFromStream(HTTP.Document);
    finally
        HTTP.Free;
        glbHTTP := nil;
    end;
end;

// переход на следующую строку таблицы
procedure TForm1.btnIncRowClick(Sender: TObject);
var
    p: integer;
    s: string;
begin
    PostNow := 0;
    ImgNow := 0;
    // проверка на заполенность строки в таблице
    while not ( (length(SG.Cells[1,StageNow + 1]) > 10) and (StrToIntDef(SG.Cells[2,StageNow + 1], -1) > 0) and
        (StrToIntDef(SG.Cells[3,StageNow + 1], -1) > 0) ) do
        begin
            inc(StageNow);
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
        s := SG.Cells[1,StageNow + 1];
        p := posex('/', s, 10);
        if p > 1 then p := p - 1;
        Edit_URL.Text := LeftStr(s, p);
        Edit_append.Text := RightStr(s, length(s) - p);
        SpinBegin.Value := StrToIntDef(SG.Cells[2,StageNow + 1], -1);
        SpinEnd.Value := StrToIntDef(SG.Cells[3,StageNow + 1], -1);
        SpinPage.Value := SpinBegin.Value;
    end;
end;

// качаем и сохраняем картинку из sURL
procedure TForm1.btnSaveImgClick(Sender: TObject);
var
    HTTP: THTTPSend;
begin
    HTTP := THTTPSend.Create;
    glbHTTP := HTTP;
    try
        SetProxy;
        HTTP.Cookies.Text := Memo_Cookies.Lines.Text;
        HTTP.Headers.Append('Referer: ' + Edit_URL.Text);
        HTTP.Sock.OnStatus := @HTTP_OnStatus;
        HTTP.HTTPMethod('GET', sURL);
        // бывает получает только 206 байт. Пробуем перескачать в таком случае
        if HTTP.Document.Size < 350 then
            HTTP.HTTPMethod('GET', sURL);
        // еще раз
        if HTTP.Document.Size < 350 then
            HTTP.HTTPMethod('GET', sURL);
        Memo_Header.Lines.Assign(HTTP.Headers);
        if (Memo_Header.Lines.Count > 3) and (HTTP.Document.Size > 349) then
            if LeftStr(Memo_Header.Lines.Strings[3], 18) <> 'Content-Type: text' then
                try
                    SaveSecond := secondof(now);
                    HTTP.Document.SaveToFile(DirNow + Edit_FileName.Text);
                    Inc(Total);
                except

                end;
    finally
        HTTP.Free;
        glbHTTP := nil;
    end;
end;

// ищем ссылки на картинки в посте
procedure TForm1.btnFindImgsClick(Sender: TObject);
var
    docLen: integer;
    s: string;
    p, p1, p2: integer;
begin
    //Memo_Imgs.Clear;
    s := Memo_Doc.Text;
    docLen := length(s);
    p := posex('<div class="image">', s);
    if p <> 0 then
    begin
        p1 := p + 19;
        p := posex('<', s, p1);
    end;
    while p <> 0 do
    begin
        //p1 := p + 1;
        Inc(imgs);
        if docLen < (p + 20) then
            Break;
        // смотрим, не ссылка ли это на большую картинку
        if MidStr(s, p, 9) = '<a href="' then
        begin
            p1 := p + 9;
            p2 := posex('"', s, p1);
            if p2 = 0 then
                p := 0
            else
            begin
                Memo_Imgs.Append(midstr(s, p1, p2 - p1));
                p := posex('<div class="image">', s, p1);
                if p <> 0 then
                begin
                    p1 := p + 19;
                    p := posex('<', s, p1);
                end;
            end;
        end
        // или это картинка
        else if MidStr(s, p, 10) = '<img src="' then
        begin
            p1 := p + 10;
            p2 := posex('"', s, p1);
            if p2 = 0 then
                p := 0
            else
            begin
                Memo_Imgs.Append(midstr(s, p1, p2 - p1));
                p := posex('<div class="image">', s, p1);
                if p <> 0 then
                begin
                    p1 := p + 19;
                    p := posex('<', s, p1);
                end;
            end;
        end
        else
        begin
            p := posex('<div class="image">', s, p1);
            if p <> 0 then
            begin
                p1 := p + 19;
                p := posex('<', s, p1);
            end;
        end;
    end;
end;

// поиск постов на странице
procedure TForm1.btnFindPostsClick(Sender: TObject);
var
    docLen: integer;
    s: string;
    p, p1, p2: integer;
begin
    //Memo_Posts.Clear;
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
        Inc(imgs);
        if docLen < (p1 + 20) then
            Break;
        // смотрим, не ссылка ли это на большую картинку

        p2 := posex('"', s, p1);
        if p2 = 0 then
            p := 0
        else
        begin
            Memo_Posts.Append('/post/' + midstr(s, p1, p2 - p1));
            p := posex('<a href="/post/', s, p1);
        end;
    end;
end;

// кнопка старт
procedure TForm1.btnStartClick(Sender: TObject);
begin
    Stop := False;
    btnStart.Enabled := false;

    if (SpinPage.Value < SpinBegin.Value) or (SpinPage.Value > SpinEnd.Value) or (SpinPage.Value = 0) then
    begin
        ImgNow := 0;
        PostNow := 0;
    end;

    StageNow := SpinRow.Value;
    if (StageNow < 0) or (StageNow > 99) then
    begin
        StageNow := 0;
        SpinPage.Value := 0;
        btnIncRowClick(nil);
    end;
    if SpinPage.Value = 0 then
        btnIncRowClick(nil);
    if StageNow < 0 then exit;
    if SpinPage.Value = 0 then
        SpinPage.Value := SpinBegin.Value;
    timer1.Interval := SpinTimer.Value;
    Timer1.Enabled := true;
    StatusBar1.Panels.Items[0].Text := 'Run';
    SpinRow.Enabled := false;
    try
        SG.SaveToCSVFile('list.csv', ';');
    except

    end;
    Application.Title := inttostr(StageNow) + ':' + inttostr(SpinPage.Value) + '/' + inttostr(SpinEnd.Value);
    if not DirectoryExistsUTF8('Pic/') then
        try
            CreateDirUTF8('Pic/');
        except

        end;
end;

// кнопка стоп
procedure TForm1.btnStopClick(Sender: TObject);
begin
    Stop := True;
    btnStart.Enabled := true;
    Timer1.Enabled := false;
    SpinRow.Enabled := true;
    StatusBar1.Panels.Items[0].Text := 'Stop';
    Application.Title := 'JoySave';
end;

// найти ссылки на гифки в посте
procedure TForm1.btnFindGifsClick(Sender: TObject);
var
    docLen: integer;
    s: string;
    p, p1, p2: integer;
begin
    //Memo_Imgs.Clear;
    s := Memo_Doc.Text;
    docLen := length(s);
    p := posex('<span class="video_gif_holder">', s);
    if p <> 0 then
    begin
        p1 := p + 31;
        p := posex('<', s, p1);
    end;
    while p <> 0 do
    begin
        //p1 := p + 31;
        if docLen < (p + 20) then
            Break;
        // смотрим, не ссылка ли это на большую картинку
        if MidStr(s, p, 9) = '<a href="' then
        begin
            p1 := p + 9;
            p2 := posex('"', s, p1);
            if p2 = 0 then
                p := 0
            else
            begin
                Memo_Imgs.Append(midstr(s, p1, p2 - p1));
                p := posex('<span class="video_gif_holder">', s, p1);
                if p <> 0 then
                begin
                    p1 := p + 31;
                    p := posex('<', s, p1);
                end;
            end;
        end
        else
        begin
            p := posex('<span class="video_gif_holder">', s, p1);
            if p <> 0 then
            begin
                p1 := p + 31;
                p := posex('<', s, p1);
            end;
        end;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    try
        SG.SaveToCSVFile('list.csv', ';');
        Memo_Cookies.Lines.SaveToFile('cookies.txt');
    except

    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    rsSources: TResourceStream;
    uz: TUnZipper;
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
            rsSources := TResourceStream.Create(HINSTANCE, 'SOURCES',  RT_RCDATA);
            try
                rsSources.SaveToFile('JoySave_sources.zip');
            finally
                FreeAndNil(rsSources);
            end;
            rsSources := TResourceStream.Create(HINSTANCE, 'LIBS',  RT_RCDATA);
            try
                rsSources.SaveToFile('openssl-1.0.2u-i386-win32.zip');
            finally
                FreeAndNil(rsSources);
            end;
        end;
    Memo_Doc.Clear;
    Memo_Header.Clear;
    Memo_Imgs.Clear;
    Memo_Posts.Clear;
    Edit_URL.Text := '';
    Edit_FileName.Text := '';
    Edit_proxyHost.Text := '';
    Edit_ProxyPort.Text := '';
    Edit_append.Text := '';
end;

procedure TForm1.IniPropStorage1RestoreProperties(Sender: TObject);
var
    i: integer;
begin
    try
        SG.LoadFromCSVFile('list.csv', ';');
    except

    end;
    try
        Memo_Cookies.Lines.LoadFromFile('cookies.txt');
    except
        Memo_Cookies.Clear;
    end;
    SG.RowCount := 101;
    if SG.ColCount < 5 then SG.ColCount := 5;
    for i := 0 to 9 do
        SG.Cells[0, i+1] := '0' + inttostr(i);
    for i := 10 to SG.RowCount - 2 do
        SG.Cells[0, i+1] := inttostr(i);
end;

procedure TForm1.SpinTimerChange(Sender: TObject);
begin
    Timer1.Interval := SpinTimer.Value;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
    p, p1: integer;
    i: integer;
    s: string;
    oz: TZipper;
    TheFileList: TStringList;
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
            if SpinPage.Value > SpinEnd.Value then
            begin
                inc(StageNow);
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
            end;
        end
        // посты получены
        else
        begin
            sURL := Edit_URL.Text + Memo_Posts.Lines.Strings[PostNow - 1];
            Inc(PostNow);
            Edit_FileName.Text := DecodeURL(sURL);
            btnGetHTMLClick(nil);
            btnFindImgsClick(nil);
            btnFindGifsClick(nil);
            if PostNow > Memo_Posts.Lines.Count then
            begin
                PostNow := 0;
                if Memo_Imgs.Lines.Count > 0 then
                    ImgNow := 1;

                SubDir := SG.Cells[4, StageNow+1];
                if SubDir = '' then SubDir := SG.Cells[0, StageNow+1];
                if not DirectoryExistsUTF8('Pic/' + SubDir + '/') then
                    try
                        CreateDirUTF8('Pic/' + SubDir + '/');
                    except

                    end;
                PrevDir := DirNow;
                DirNow := 'Pic/' + SubDir + '/' +
                    inttostr((SpinPage.Value div SpinPageCount.Value) * SpinPageCount.Value) + '/';
                if not DirectoryExistsUTF8(DirNow) then
                    try
                        CreateDirUTF8(DirNow);
                    except

                    end;
                if (PrevDir <> '') and (PrevDir <> 'Pic/') and (PrevDir <> DirNow) and cbPackToCBZ.Checked then
                begin
                    oz := TZipper.Create;
                    oz.FileName := leftstr(PrevDir, Length(PrevDir)-1) + '.cbz';
                    TheFileList := TStringList.Create;
                    try
                        FindAllFiles(TheFileList, PrevDir);
                        oz.UseLanguageEncoding := true;
                        oz.Entries.AddFileEntries(TheFileList);
                        for i := 0 to oz.Entries.Count - 1 do
                        begin
                            oz.Entries.Entries[i].CompressionLevel := clnone;
                            s := oz.Entries.Entries[i].DiskFileName;
                            oz.Entries.Entries[i].ArchiveFileName := RightStr(s, length(s) - 5 - length(SubDir));
                        end;
                        oz.ZipAllFiles;
                        if cbDelAfterPack.Checked then DeleteDirectory(PrevDir, false);
                    finally
                        TheFileList.Free;
                        oz.Free;
                    end;

                end;
                SpinPage.Value := SpinPage.Value + 1;
                Application.Title := inttostr(StageNow) + ':' + inttostr(SpinPage.Value) + '/' + inttostr(SpinEnd.Value);
            end;
        end
    else
    begin
        sURL := Memo_Imgs.Lines.Strings[ImgNow - 1];
        if length(sURL) > 2 then
            if (sURL[1] = '/') and (sURL[2] = '/') then sURL := 'https:' + sURL;
        s := DecodeURL(sURL);
        p := posex('/', s, 1);
        p1 := p;
        while p <> 0 do
        begin
            p1 := p;
            p := posex('/', s, p + 1);
        end;
        Edit_FileName.Text := RightStr(s, Length(s) - p1);
        if not FileExistsUTF8(DirNow + Edit_FileName.Text) then
            btnSaveImgClick(nil);


        Inc(ImgNow);
        if ImgNow > Memo_Imgs.Lines.Count then
            ImgNow := 0;
    end;
    lblImagesOnPage.Caption := IntToStr(imgs);
    StatusBar1.Panels.Items[1].Text := 'Tot: ' + IntToStr(Total);
    StatusBar1.Panels.Items[2].Text := 'Post: ' + IntToStr(PostNow);
    StatusBar1.Panels.Items[3].Text := 'Img: ' + IntToStr(ImgNow) + '/' + IntToStr(imgs);
    timer1.Enabled := True;
end;


procedure TForm1.HTTP_OnStatus(Sender: TObject; Reason: THookSocketReason;
	const Value: String);
begin
    if assigned(glbHTTP) then
    begin
        if Stop then glbHTTP.Abort;
    end;
    Application.ProcessMessages;
end;

procedure TForm1.SetProxy;
begin
    if rgProxy.ItemIndex = 0 then
    begin
        glbHTTP.Sock.SocksIP := '';
        glbHTTP.ProxyHost := '';
    end;
    if rgProxy.ItemIndex = 1 then
    begin
        glbHTTP.ProxyHost := Edit_proxyHost.Text;
        glbHTTP.ProxyPort := Edit_ProxyPort.Text;
        glbHTTP.ProxyUser := Edit_proxyUser.Text;
        glbHTTP.ProxyPass := Edit_ProxyPass.Text;
        glbHTTP.Sock.SocksIP := '';
    end;
    if rgProxy.ItemIndex > 1 then
    begin
        glbHTTP.ProxyHost := '';
        glbHTTP.Sock.SocksIP := Edit_proxyHost.Text;
        glbHTTP.Sock.SocksPort := Edit_ProxyPort.Text;
        glbHTTP.Sock.SocksUsername := Edit_proxyUser.Text;
        glbHTTP.Sock.SocksPassword := Edit_ProxyPass.Text;
        glbHTTP.Sock.SocksResolver := true;
    end;
    glbHTTP.Sock.CreateWithSSL(TSSLOpenSSL);
    if rgProxy.ItemIndex = 2 then
        glbHTTP.Sock.SocksType := ST_Socks4;
    if rgProxy.ItemIndex = 3 then
        glbHTTP.Sock.SocksType := ST_Socks5;
end;

end.
