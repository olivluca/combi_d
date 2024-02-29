unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniPropStorage, Serial, trumautils;

type

  { TLogger }

  TLogger = class(TThread)
   FPort:THandle;
    FFile:textfile;
    FLogMessage:string;
    FAddToLog:boolean;
    FFrameCount:integer;
    function GetFrameCount: integer;
    procedure Log(const msg: string);
    constructor Create(const PortName,Filename:String);
    destructor Destroy;override;
    procedure Execute;override;
    procedure AddToLog(const msg:string);
    function Opened:boolean;
    property FrameCount:integer read GetFrameCount;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnStartStop: TButton;
    btnAddmessage: TButton;
    comPort: TEdit;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    LabelFrames: TLabel;
    LogMessage: TComboBox;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure btnAddmessageClick(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IniPropStorage1RestoreProperties(Sender: TObject);
    procedure LogMessageChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FLogger:TLogger;
    procedure ClearLogmessage;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TLogger }

function TLogger.GetFrameCount: integer;
begin
  result:=InterlockedExchange(FFrameCount,FFrameCount);
end;

procedure TLogger.Log(const msg: string);
begin
  writeln(FFile,GetTickCount64:12,' ',msg);
end;

constructor TLogger.Create(const PortName, Filename: String);
begin
  AssignFile(FFile,Filename);
  Rewrite(FFile);
  FPort:=SerOpen(Portname);
  if Opened then
    SerSetParams(FPort,9600,8,NoneParity,1,[]);
  inherited create(false);
end;

destructor TLogger.Destroy;
begin
  inherited Destroy;
  CloseFile(FFile);
  if Opened then
    SerClose(FPort);
end;

procedure TLogger.Execute;
var
  status:(WaitBreak,WaitSync,WaitPid, WaitData);
  lindata:string;
  c:byte;
  id,pid:byte;
  oldtick,framestart: QWord;
  checksumreceived:boolean;
  checksum:byte;
  bytesreceived:integer;

  procedure LogFrame;
  var
    i: Integer;
    locframe: String;
    calcchecksum: UInt8;
  begin
    InterlockedIncrement(FFrameCount);
    Write(FFile,framestart:12);
    Write(FFile,' id ');
    Write(FFile,IntToHex(id,2));
    Write(FFile,' pid ');
    Write(FFile,IntToHex(pid,2));
    Write(FFile,' : ');
    for i:=1 to 8 do
    begin
      if i>bytesreceived then
        Write(FFile,'  ')
      else
        Write(FFile,IntToHex(byte(lindata[i]),2));
      write(FFile,' ')
    end;
    if bytesreceived=0 then
      writeln(FFile, 'no data')
    else
    begin
      if checksumreceived then
      begin
        locframe:=lindata;
        SetLength(locframe,bytesreceived);
        calcchecksum:=GetChecksum(pid,locframe);
        if calcchecksum=checksum then
          writeln(FFile,'')
        else
          writeln(FFile,'*** checksum expected ',inttohex(calcchecksum,2),' received ',IntToHex(checksum,2))
      end else
      begin
        writeln(FFile,'*** no checksum received')
      end;
    end;
  end;

begin
  Log('capture started');
  if not Opened then
  begin
    Log('port not open, capture stopped');
    exit;
  end;
  status:=WaitBreak;
  setlength(lindata,8);
  oldtick:=GetTickCount64;
  while not terminated do
  begin
    if FAddToLog then
    begin
      Writeln(FFile,'====================== ',FLogMessage);
      FAddToLog:=false;
    end;
    if SerReadTimeout(FPort,c,1,50)<>1 then
    begin
      if status=WaitData then
      begin
        LogFrame
      end else
      if GetTickCount64-oldtick>=5000 then
      begin
        Log('not receiving');
        oldtick:=GetTickCount64;
      end;
      Status:=WaitBreak;
      continue;
    end;
    oldtick:=GetTickCount64;
    case Status of
     WaitBreak:
       begin
         if c=0 then
         begin
           framestart:=oldtick;
           Status:=WaitSync;
         end else
         begin
           Log('waiting break, discarding '+inttohex(c,2));
         end;
       end;
     WaitSync:
       begin
         if c=$55 then
         begin
           Status:=WaitPid;
         end else
         begin
           Log('waiting sync, discarding '+inttohex(c,2));
           Status:=WaitBreak;
         end;
       end;
     WaitPid:
       begin
         pid:=c;
         id:=pid and $3f;
         bytesreceived:=0;
         checksumreceived:=false;
         Status:=WaitData;
       end;
     WaitData:
       begin
         if bytesreceived=8 then
         begin
           checksum:=c;
           checksumreceived:=true;
           LogFrame;
           Status:=WaitBreak;
         end else
         begin
           bytesreceived:=bytesreceived+1;
           byte(lindata[bytesreceived]):=c
         end;
       end;
    end;
  end;
end;

procedure TLogger.AddToLog(const msg: string);
begin
  //I don't think it could happen, but...
  if FAddToLog then
    exit;
  FLogMessage:=msg;
  FAddToLog:=true;
end;

function TLogger.Opened: boolean;
begin
  Result:=FPort>0;
end;

{ TForm1 }

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  LogMessage.Enabled:=false;
  ClearLogmessage;
  if Assigned(FLogger) then
  begin
    Timer1.Enabled:=false;
    FreeAndNil(FLogger);
    btnStartStop.Caption:='Start';
  end else
  begin
    if SaveDialog1.Execute then
    begin
      FLogger:=TLogger.Create(comPort.text,SaveDialog1.FileName);
      if FLogger.Opened then
      begin
        LogMessage.Enabled:=true;
        LabelFrames.Caption:='0';
        btnStartStop.Caption:='Stop';
        Timer1.Enabled:=true
      end else
      begin
        MessageDlg('Error','Cannot open serial port',mtError,[mbOk],0);
        FreeAndNil(FLogger)
      end;
    end;
  end;
end;

procedure TForm1.btnAddmessageClick(Sender: TObject);
begin
  FLogger.AddToLog(LogMessage.Text);
  ClearLogmessage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLogger);
end;

procedure TForm1.IniPropStorage1RestoreProperties(Sender: TObject);
begin
  if comPort.Text='' then
    {$IFDEF unix}
    comport.Text:='/dev/tty???'
    {$ELSE}
    comport.Text:='\\.\COM??'
    {$ENDIF}
end;

procedure TForm1.LogMessageChange(Sender: TObject);
begin
  btnAddmessage.Enabled:=LogMessage.Text<>'';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  LabelFrames.Caption:=IntToStr(FLogger.FrameCount);
end;

procedure TForm1.ClearLogmessage;
begin
  LogMessage.Text:='';
  btnAddMessage.enabled:=false;
end;

end.

