unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  MaskEdit, ExtCtrls, receiver;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnPlayPause: TButton;
    BtnRestartReplay: TButton;
    CbOnOff: TCheckBox;
    AutoTemp: TCheckBox;
    ReplyToFrame14: TCheckBox;
    Error: TCheckBox;
    Timer1: TTimer;
    WaterDemand: TCheckBox;
    RoomDemand: TCheckBox;
    Fenster: TCheckBox;
    Netz: TCheckBox;
    Easi: TCheckBox;
    Diag1: TMaskEdit;
    Diag2_1: TMaskEdit;
    Diag2_2: TMaskEdit;
    Diag2_3: TMaskEdit;
    Label10: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Diag1h: TLabel;
    Label8: TLabel;
    Diag2h: TLabel;
    Label9: TLabel;
    Temperature: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Fan: TLabel;
    Memo1: TMemo;
    Voltage: TFloatSpinEdit;
    Water: TFloatSpinEdit;
    TempSetpoint: TLabel;
    WaterSetpoint: TLabel;
    procedure AutoTempChange(Sender: TObject);
    procedure BtnPlayPauseClick(Sender: TObject);
    procedure Diag1Change(Sender: TObject);
    procedure Diag2_1Change(Sender: TObject);
    procedure BitChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReplyToFrame14Change(Sender: TObject);
    procedure BtnRestartReplayClick(Sender: TObject);
    procedure TemperatureChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VoltageChange(Sender: TObject);
    procedure WaterChange(Sender: TObject);
  private
    FReceiver:TTrumaReceiver;
    FDiag2:TDiagReply2;
    FStatusByte:byte;
    procedure OnFan(const Afan: byte);
    procedure Message(const msg: string);
    procedure OnOff(const AOnOff: boolean);
    procedure Setpoint(const awater: boolean; setpoint: extended);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  port: String;
  replay: String;
begin
  port:=Application.GetOptionValue('port');
  replay:=Application.GetOptionValue('replay');
  if port='' then
    port:='/dev/ttyUSB0';
  FReceiver:=TTrumaReceiver.Create(port,replay,@Setpoint, @OnFan, @OnOff, @Message,Application.HasOption('nolin'),Application.HasOption('gas'));
  if not FReceiver.Opened then
  begin
    MessageDlg('Error','Cannot open port '+port,mtError,[mbOk],0);
    Application.ShowMainForm:=false;
    Application.Terminate;
  end;
  BtnPlayPause.Visible:=FReceiver.ReplayOk;
  BtnRestartReplay.Visible:=FReceiver.ReplayOk;
end;

procedure TForm1.Diag1Change(Sender: TObject);
var
  b: Longint;
begin
  if TryStrToInt('$'+Diag1.Text,b) then
  begin
    FReceiver.DiagReply1:=b;
    Diag1H.Caption:=IntToHex(b,2);
  end;
end;

procedure TForm1.AutoTempChange(Sender: TObject);
begin
  TImer1.enabled:=AutoTemp.Checked;
end;

procedure TForm1.BtnPlayPauseClick(Sender: TObject);
var
  playing: Boolean;
begin
  BtnPlayPause.Tag:=1-BtnPlayPause.Tag;
  playing:=BtnPlayPause.Tag=0;
  if playing then
    BtnPlayPause.Caption:='Pause'
  else
    BtnPlayPause.Caption:='Play';
  FReceiver.Playing:=playing;
end;

procedure TForm1.Diag2_1Change(Sender: TObject);
var
  s: TMaskEdit;
  b: Longint;
begin
  s:=TMaskEdit(Sender);
  if TryStrToInt('$'+s.Text,b) then
  begin
    FDiag2[s.tag]:=b;
    Diag2h.Caption:=format('%.2x, %.2x, %.2x', [FDiag2[1],FDiag2[2],FDiag2[3]]);
    FReceiver.DiagReply2:=FDiag2;
  end;
end;

procedure TForm1.BitChange(Sender: TObject);
var
  c: TCheckBox;
  mask:byte;
begin
  c:=TCheckBox(sender);
  mask:=1 shl c.tag;
  if c.checked then
    FStatusByte:=FStatusByte or mask
  else
    FStatusByte:=FStatusByte and not mask;
  FReceiver.StatusByte:=FStatusByte;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FReceiver.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FReceiver.Start;
end;

procedure TForm1.ReplyToFrame14Change(Sender: TObject);
begin
  FReceiver.ReplyToFrame14:=ReplyToFrame14.Checked;
end;

procedure TForm1.BtnRestartReplayClick(Sender: TObject);
begin
  FReceiver.RestartReplay;
end;

procedure TForm1.TemperatureChange(Sender: TObject);
begin
  FReceiver.Temperature:=Temperature.Value;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  increment, newvalue: Extended;
begin
  if timer1.tag=0 then
    increment:=0.1
  else
    increment:=-0.1;
  newvalue:=Temperature.Value+increment;
  if (newvalue>30.0) or (newvalue<0.0) then
  begin
    timer1.tag:=1-timer1.tag;
  end else
   Temperature.Value:=newvalue;

end;

procedure TForm1.VoltageChange(Sender: TObject);
begin
  FReceiver.Voltage:=Voltage.Value;
end;

procedure TForm1.WaterChange(Sender: TObject);
begin
  FReceiver.TemperatureWater:=Water.Value;
end;

procedure TForm1.Setpoint(const awater: boolean; setpoint: extended);
begin
  if awater then
    WaterSetpoint.Caption:=Format('%1.1f',[setpoint])
  else
    TempSetpoint.Caption:=Format('%1.1f',[setpoint]);
end;

procedure TForm1.OnFan(const Afan: byte);
begin
  Fan.Caption:=IntToHex(Afan,2);
end;

procedure TForm1.Message(const msg: string);
begin
  Memo1.Lines.Add(datetimetostr(now)+' '+msg);
  Memo1.VertScrollBar.Position:=Memo1.VertScrollBar.Size+10000;
end;

procedure TForm1.OnOff(const AOnOff: boolean);
begin
  CbOnOff.Checked:=AOnOff
end;

end.

