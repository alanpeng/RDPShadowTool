unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, Windows,
  JwaWinType, JwaWtsApi32, JwaWinsock2, ShellAPI, StrUtils, UnitAbout,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation;

type
  PWtsSessionInfoAArray = ^TWtsSessionInfoAArray;
  TWtsSessionInfoAArray = array [0 .. ANYSIZE_ARRAY - 1] of WTS_SESSION_INFOA;

  TFormMain = class(TForm)
    ButtonRefresh: TButton;
    ComboBoxSessions: TComboBox;
    LabelSessions: TLabel;
    ButtonRemoteView: TButton;
    ButtonRemoteControl: TButton;
    ButtonAbout: TButton;
    ButtonClose: TButton;
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonRemoteViewClick(Sender: TObject);
    procedure ButtonRemoteControlClick(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

function WinExecAndWait(Filename: string; Visibility: integer): integer;
var
  zAppName: array [0 .. 512] of char;
  zCurDir: array [0 .. 255] of char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, Filename);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if CreateProcess(nil, zAppName, { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    False, { handle inheritance flag }
    CREATE_NEW_CONSOLE or { creation flags }
    NORMAL_PRIORITY_CLASS, nil, { pointer to new environment block }
    nil, { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
    Result := 0;
  end
  else
    Result := -1; { pointer to PROCESS_INF }
end;

function GetSessionUserName(dwSessionId: DWORD): string;
var
  bRes: Boolean;
  dwBufferLen: DWORD;
  Ptr: PWtsClientAddress;
begin
  bRes := WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, dwSessionId, WTSUserName, Pointer(Ptr), dwBufferLen);
  if bRes = False then
  begin
    Result := '';
    exit;
  end;
  Result := PWideChar(Ptr);
end;

procedure ListActiveSessions;
var
  hServer: THandle;
  SessionInfoPtr: PWtsSessionInfoAArray;
  pCount: Cardinal;
  ClientAddressPtr: PWtsClientAddress;
  dwBytesReturned: Cardinal;
  IPStr: String;
  i: integer;
begin
  hServer := WTS_CURRENT_SERVER;
  if WtsEnumerateSessions(hServer, 0, 1, PWTS_SESSION_INFO(SessionInfoPtr), pCount) then
  begin
    for i := 0 to pCount - 1 do
    begin
      WTSQuerySessionInformation(hServer, SessionInfoPtr^[i].SessionId, WTSClientAddress, Pointer(ClientAddressPtr),
        dwBytesReturned);
      case ClientAddressPtr^.AddressFamily of
        AF_INET:
          IPStr := Format('%d.%d.%d.%d', [ClientAddressPtr^.Address[2], ClientAddressPtr^.Address[3],
            ClientAddressPtr^.Address[4], ClientAddressPtr^.Address[5]]);
        AF_INET6:
          IPStr := 'IPv6 address not yet supported';
        AF_IPX:
          IPStr := 'IPX is not supported';
        AF_NETBIOS:
          IPStr := 'NETBIOS is not supported';
        AF_UNSPEC:
          IPStr := 'Unspecified';
      end;
      if IPStr <> 'Unspecified' then
      begin
        FormMain.ButtonRemoteView.Enabled := True;
        FormMain.ButtonRemoteControl.Enabled := True;
        FormMain.ComboBoxSessions.ItemIndex := 0;
        FormMain.ComboBoxSessions.Items.Add(Format('Session: %d | User: ', [SessionInfoPtr^[i].SessionId]) +
          GetSessionUserName(SessionInfoPtr^[i].SessionId));
      end;
      WTSfreeMemory(ClientAddressPtr);
    end;
  end;
  WTSfreeMemory(SessionInfoPtr);
end;

procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TFormMain.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ButtonRefreshClick(Sender: TObject);
begin
  ComboBoxSessions.Items.Clear;
  ListActiveSessions;
end;

procedure TFormMain.ButtonRemoteControlClick(Sender: TObject);
var
  SessionId: String;
begin
  SessionId := ComboBoxSessions.Selected.Text;
  SessionId := LeftStr(SessionId, Pos('|', SessionId) - 1);
  SessionId := RightStr(SessionId, Length(SessionId) - Pos(' ', SessionId) + 1);
  FormMain.ButtonRemoteControl.Enabled := False;
  Application.ProcessMessages;
  if WinExecAndWait('mstsc /control /shadow:' + SessionId,1)= -1 then
    MessageBox(0,'调用测试程序"mstsc.exe"失败或找不到该程序！','错误：',MB_ICONERROR);
  Application.ProcessMessages;
  FormMain.ButtonRemoteControl.Enabled := True;
end;

procedure TFormMain.ButtonRemoteViewClick(Sender: TObject);
var
  SessionId: String;
begin
  SessionId := ComboBoxSessions.Selected.Text;
  SessionId := LeftStr(SessionId, Pos('|', SessionId) - 1);
  SessionId := RightStr(SessionId, Length(SessionId) - Pos(' ', SessionId) + 1);
  FormMain.ButtonRemoteView.Enabled := False;
  Application.ProcessMessages;
  if WinExecAndWait('mstsc /shadow:' + SessionId,1)= -1 then
    MessageBox(0,'调用测试程序"mstsc.exe"失败或找不到该程序！','错误：',MB_ICONERROR);
  Application.ProcessMessages;
  FormMain.ButtonRemoteView.Enabled := True;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  ListActiveSessions;
end;

end.
