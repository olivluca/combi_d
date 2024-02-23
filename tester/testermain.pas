unit testermain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Spin, ExtCtrls, truma;

type

  { TForm1 }

  TForm1 = class(TForm)
    BoilerMode: TComboBox;
    FanBoost: TCheckBox;
    OnOff: TCheckBox;
    FanMode: TComboBox;
    Heat: TCheckBox;
    FanSpeed: TSpinEdit;
    Memo1: TMemo;
    TempSetpoint: TFloatSpinEdit;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure FanBoostChange(Sender: TObject);
    procedure FanModeChange(Sender: TObject);
    procedure FanSpeedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HeatChange(Sender: TObject);
    procedure BoilerModeChange(Sender: TObject);
    procedure OnOffChange(Sender: TObject);
    procedure TempSetpointChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FTruma:TTrumaD;
    procedure NewDiag(const index: integer; const payload: string);
    procedure NewMessage(const msg: string);
    procedure NewStatus(const index: integer; const payload: string);
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
begin
  port:=Application.GetOptionValue('port');
  if port='' then
    port:='/dev/ttyUSB0';
  FTruma:=TTrumaD.create(port,@NewStatus,@NewDiag,@NewMessage);
  if Application.HasOption('fakereceive') then
    FTruma.FakeReceive:=true;
  if not FTruma.Opened then
  begin
    MessageDlg('Error','Impossible to open port',mtError,[mbOk],0);
    Application.Terminate;
  end;
end;

procedure TForm1.FanSpeedChange(Sender: TObject);
begin
  FTruma.FanSpeed:=FanSpeed.Value;
end;

procedure TForm1.FanModeChange(Sender: TObject);
begin
  FTruma.FanMode:=TFanMode(FanMode.ItemIndex);
end;

procedure TForm1.FanBoostChange(Sender: TObject);
begin
  FTruma.FanBoost:=FanBoost.Checked;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTruma.Free;
end;

procedure TForm1.HeatChange(Sender: TObject);
begin
  if not Heat.Checked then
    FTruma.SetPointTemp:=0.0
  else
    FTruma.SetPointTemp:=TempSetpoint.Value;

end;

procedure TForm1.BoilerModeChange(Sender: TObject);
begin
  FTruma.BoilerMode:=TBoilerMode(BoilerMode.ItemIndex);
end;

procedure TForm1.OnOffChange(Sender: TObject);
begin
  FTruma.OnOff:=OnOff.Checked;
end;

procedure TForm1.TempSetpointChange(Sender: TObject);
begin
  if Heat.Checked then
    FTruma.SetPointTemp:=TempSetpoint.Value;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  //FTruma.Loop
end;

function ShowBit(by,bi:byte):string;
begin
  if ((by shr bi) and 1)<>0 then
    result:='1'
  else
    result:='0';
end;
procedure TForm1.NewStatus(const index: integer; const payload: string);

var
  bits: Byte;
begin
  with StringGrid1 do
  begin
    //0 is extended flags, always 0
    bits:=byte(payload[2]);
    Cells[1,0]:=ShowBit(bits,0);
    Cells[1,1]:=ShowBit(bits,1);
    Cells[1,2]:=ShowBit(bits,2);
    Cells[1,3]:=ShowBit(bits,3);
    Cells[1,4]:=ShowBit(bits,4);
    Cells[1,5]:=ShowBit(bits,7);
    Cells[1,6]:=Format('%.1f ºC',[DecodeTempKelvin(byte(payload[3]),byte(payload[4]))]);
    Cells[1,7]:=Format('%.1f ºC',[DecodeTempKelvin(byte(payload[5]),byte(payload[6]))]);
    Cells[1,8]:=Format('%.1f V',[DecodeVoltage(byte(payload[7]),byte(payload[8]))]);
  end;
end;

procedure TForm1.NewDiag(const index: integer; const payload: string);
var
  x: Integer;
begin
  with StringGrid1 do
  begin
    if index=1 then
    begin
      for x:=0 to 1 do
        Cells[1,9+x]:=IntToHex(byte(payload[4+x]))
    end else
    begin
      for x:=0 to 3 do
        Cells[1,11+x]:=IntToHex(byte(payload[4+x]))
    end;
  end;
end;

procedure TForm1.NewMessage(const msg: string);
begin

  Memo1.Lines.Add(datetimetostr(now)+' '+msg);
  Memo1.VertScrollBar.Position:=Memo1.VertScrollBar.Size+10000;

end;

end.

