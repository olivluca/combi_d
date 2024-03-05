unit testermain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Spin, ExtCtrls, ValEdit, truma, trumautils;

type

  { TForm1 }

  TForm1 = class(TForm)
    BoilerMode: TComboBox;
    btnReset: TButton;
    cbSimulateTemp: TCheckBox;
    Label1: TLabel;
    ReadCycles: TLabel;
    OnOff: TCheckBox;
    FanMode: TComboBox;
    Heat: TCheckBox;
    FanSpeed: TSpinEdit;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    TempSetpoint: TFloatSpinEdit;
    TempSimulation: TFloatSpinEdit;
    Timer1: TTimer;
    ValueListEditor1: TValueListEditor;
    procedure btnResetClick(Sender: TObject);
    procedure cbSimulateTempChange(Sender: TObject);
    procedure FanModeChange(Sender: TObject);
    procedure FanSpeedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HeatChange(Sender: TObject);
    procedure BoilerModeChange(Sender: TObject);
    procedure OnOffChange(Sender: TObject);
    procedure TempSetpointChange(Sender: TObject);
    procedure TempSimulationChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FTruma:TTrumaD;
    FReadCycles:integer;
    procedure NewDiag(const index: integer; const payload: string);
    procedure NewMessage(const msg: string);
    procedure NewFrame(const id: byte);
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
  FTruma:=TTrumaD.create(port,@NewFrame,@NewDiag,@NewMessage);
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

procedure TForm1.btnResetClick(Sender: TObject);
begin
  FTruma.ResetError;
end;

procedure TForm1.cbSimulateTempChange(Sender: TObject);
begin
  if cbSimulateTemp.Checked then
    FTruma.SimulTemp:=TempSimulation.Value
  else
    FTruma.SimulTemp:=-273.0
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

procedure TForm1.TempSimulationChange(Sender: TObject);
begin
  if cbSimulateTemp.checked then
     FTruma.SimulTemp:=TempSimulation.Value;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  //FTruma.Loop
end;

function ShowBit(value:boolean):string;
begin
  if value then
    result:='1'
  else
    result:='0';
end;
procedure TForm1.NewFrame(const id:byte);
begin
  case id of
    $16:
        with ValueListEditor1,FTruma.Frame16 do
        begin
          //0 is extended flags, always 0
          Values['16 antifreeze']:=ShowBit(Antifreeze);
          Values['16 220V supply']:=ShowBit(Supply220);
          Values['16 window']:=ShowBit(Window);
          Values['16 room demand']:=ShowBit(RoomDemand);
          Values['16 water demand']:=ShowBit(WaterDemand);
          Values['16 error']:=ShowBit(Error);
          Values['16 room temp.']:=FormatFloat('0.0',RoomTemperature);
          Values['16 water temp.']:=FormatFloat('0.0',WaterTemperature);
          Values['16 battery voltage']:=FormatFloat('0.0',BatteryVoltage);
          FReadCycles:=FReadCycles+1;
          ReadCycles.Caption:=IntToStr(FReadCycles);
        end;
    $14:
        With ValueListEditor1, FTruma.Frame14 do
        begin
          Values['14 Room temp']:=FormatFloat('0.0',RoomTemperature);
          Values['14 Room target']:=FormatFloat('0.0',RoomTargetTemperature);
          Values['14 Water target']:=FormatFloat('0.0',WaterTargetTemperature);
        end;
    $34:
        With ValueListEditor1, FTruma.Frame34 do
        begin
          Values['34 Op.time']:=IntToStr(OperationTime);
          Values['34 Relays']:=Format('K1: %d, K2: %d, K3: %d',[ord(RelayK1),ord(RelayK2),ord(RelayK3)]);
          Values['34 EBT mode']:=EbtMode;
          Values['34 Hydr.start info']:=HydronicStartInfo;
        end;
    $37:
        With ValueListEditor1, FTruma.Frame37 do
        begin
          Values['37 Trend value hydronic']:=FormatFloat('0.0',TrendValueHydronic);
        end;
     $39:
        With ValueListEditor1, FTruma.Frame39 do
        begin
          Values['39 Blow out temp']:=FormatFloat('0.0',BlowOutTemperature);
          Values['39 Pump freq.']:=FormatFloat('0.0',PumpFrequency);
          Values['39 Flame temp']:=FormatFloat('0.0',FlameTemperature);
        end;
     $35:
        With ValueListEditor1, FTruma.Frame35 do
        begin
          Values['35 Burner fan V']:=FormatFloat('0.0',BurnerFanVoltage);
          Values['35 Status']:=BurnerStatus;
          Values['35 Glow plug']:=GlowPlugStatus;
          Values['35 Hydonic state']:=HydronicState;
          Values['35 Hydronic flame']:=HydronicFlame;
        end;
     $3b:
        With ValueListEditor1, FTruma.Frame3b do
        begin
          Values['3b Battery']:=FormatFloat('0.0',Battery);
          Values['3b Extractor fan rpm']:=IntToStr(ExtractorFanRpm);
          Values['3b Current error hydr.']:=IntToStr(CurrentErrorHydronic);
          Values['3b Pump safety sw']:=PumpSafetySwitch;
          Values['3b Circ.Air motor setpoint']:=IntToStr(CircAirMotor_Setpoint);
          Values['3b Circ.Air motor current']:=FormatFloat('0.0',CircAirMotorCurrent);
        end;
    else
      NewMessage('received unknown frame '+IntToHex(id,2))
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
        Cells[1,x]:=IntToHex(byte(payload[1+x]))
    end else
    begin
      for x:=1 to 3 do
        Cells[1,2+x-1]:=IntToStr(byte(payload[1+x]))
    end;
  end;
end;

procedure TForm1.NewMessage(const msg: string);
begin

  Memo1.Lines.Add(datetimetostr(now)+' '+msg);
  //Memo1.VertScrollBar.Position:=Memo1.VertScrollBar.Size+10000;

end;

end.

