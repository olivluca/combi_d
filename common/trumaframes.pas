unit trumaframes;

{$mode ObjFPC}{$H+}
{$PackRecords C}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,Math;

type

  //------------------- frames read ---------------------
  { TFrame14 }

  TFrame14 = bitpacked record
  private
    function GetRoomTargetTemperature: extended;
    function GetRoomTemperature: extended;
    function GetWaterTargetTemperature: extended;
  public
    _RoomTemperature:word;                 //signum 60
    _RoomTargetTemperature: word;          //signum 59
    _WaterTargetTemperature: word;         //signum 61
    _EnergySelection: byte;                //signum 72
    _Spare: byte;
    property RoomTemperature:extended read GetRoomTemperature;
    property RoomTargetTemperature:extended read GetRoomTargetTemperature;
    property WaterTargetTemperature:extended read GetWaterTargetTemperature;
  end;

  { TFrame16 }

  TFrame16 = bitpacked record
  private
    function GetBatteryVoltage: extended;
    function GetRoomTemperature: extended;
    function GetWaterTemperature: extended;
  public
    FixInfo:byte;                     //signum 62
    Antifreeze:boolean;               //signum 63
    Supply220:boolean;                //signum 64
    Window:boolean;                   //signum 68
    RoomDemand:boolean;               //signum 65
    WaterDemand:boolean;              //signum 66
    Bit13:boolean;
    Bit14:boolean;
    Error:boolean;                    //signum 67
    _RoomTemperature:word;            //signum 69
    _WaterTemperature:word;           //signum 70
    _BatteryVoltage:word;             //signum 71
    property RoomTemperature:extended read GetRoomTemperature;
    property WaterTemperature:extended read GetWaterTemperature;
    property BatteryVoltage:extended read GetBatteryVoltage;
  end;

  { TFrame34 }

  TFrame34 = bitpacked record
  private
    function GetEbtMode: string;
    function GetHydronicStartInfo: string;
    function GetOperationTime: integer;
  public
    _Event2:byte;                                //signum 42  (livedisplay uses 46, probably a bug)
    _OperationTime:packed array[0..2] of byte;   //signum 43
    BtMode:byte;                                 //signum 4
    StatusFlags4:byte;                           //signum 23
    _EbtMode:byte;                               //signum 16
    RelayK2:boolean;                             //signum 17 (complete byte)
    RelayK1:Boolean;
    RelayK3:boolean;
    bit3:boolean;
    bit4:boolean;
    bit5:boolean;
    bit6:boolean;
    bit7:boolean;
    property OperationTime:integer read GetOperationTime;
    property EbtMode:string read GetEbtMode;
    property HydronicStartInfo:string read GetHydronicStartInfo;
  end;

  { TFrame37 }

  TFrame37 = bitpacked record
  private
    function GetTrendValueHydronic: extended;
  public
    StatusFlags2:byte;                //signum 21
    StatusFlags3:byte;                //signum 22
    DrawingNoHydronic:dword;          //signum 44
    ChangeLevelHydronic:byte;         //signum 45
    _TrendvalueHydronic:byte;          //signum 32
    property TrendValueHydronic:extended read GetTrendValueHydronic;
  end;

  { TFrame39 }

  TFrame39 = bitpacked record
  private
    function GetBlowOutTemperature: extended;
    function GetFlameTemperature: extended;
    function GetPumpFrequency: extended;
  public
    _BlowOutTemperature:word;               //signum 13
    BlowOutActualGradient:word;             //signum 14
    BlowOutGradient:word;                   //signum 15
    _FlameTemperature:byte;                 //signum 31
    _PumpFrequency:byte;                    //signum 33
    property BlowOutTemperature:extended read GetBlowOutTemperature;
    property PumpFrequency:extended read GetPumpFrequency;
    property FlameTemperature:extended read GetFlameTemperature;
  end;

  { TFrame35 }

  TFrame35 = bitpacked record
  private
    function GetBurnerFanVoltage: extended;
    function GetBurnerStatus: string;
    function GetHydronicFlame: string;
    function GetHydronicState: string;
    function GetGlowPlugStatus: string;
  public
    BatteryVoltageHydronic:byte;         //signum 34
    ConditionNrHydronic:byte;            //signum 35
    _AV3_Hydronic:byte;                  //signum 36
    _EVENT0_Hydronic:byte;               //signum 37
    SafetyTime_Hydronic:byte;            //signum 38
    StateTime_Hydronic:byte;             //signum 39
    _BurnerFanVoltage:byte;              //signum 40
    _AV2_Hydronic:byte;                  //signum 41
    property BurnerStatus:string read GetBurnerStatus;
    property GlowPlugStatus:string read GetGlowPlugStatus;
    property BurnerFanVoltage:extended read GetBurnerFanVoltage;
    property HydronicState:string read GetHydronicState;
    property HydronicFlame:string read GetHydronicFlame;
  end;

  { TFrame3b }

  TFrame3b = bitpacked record
  private
    function GetBattery: extended;
    function GetCircAirMotorCurrent: extended;
    function GetExtractorFanRpm: integer;
    function GetPumpSafetySwitch: string;
  public
    SoftwareVersion_Tech:byte;       //signum 0
    CurrentErrorTruma:byte;          //signum 2
    CurrentErrorHydronic:byte;       //signum 25
    _Battery:byte;                   //signum 3
    Hyd_power_level:byte;            //signum 5
    CircAirMotor_Setpoint:byte;      //signum 6
    _CircAirMotorCurrent:byte;       //signum 7
    _ExtractorFanRpm:byte;           //signum 8
    property ExtractorFanRpm:integer read GetExtractorFanRpm;
    property Battery:extended read GetBattery;
    property PumpSafetySwitch:string read GetPumpSafetySwitch;
    property CircAirMotorCurrent:extended read GetCircAirMotorCurrent;  //in Ampere
  end;

implementation


function GetTemperature(RawValue:word):extended;
begin
  result:=LeToN(RawValue)*0.1-273;
end;

function RawToFlameTemperature(RawValue:byte):extended;
begin
    result:=power(RawValue,3.0)* 1.8602209820528515E-05 + Power(RawValue, 2.0) * -0.0004895309102721512 + RawValue * 1.4470709562301636 -65.64685821533203;

end;

{ TFrame37 }

function TFrame37.GetTrendValueHydronic: extended;
begin
  result:=RawToFlameTemperature(_TrendvalueHydronic);
end;

{ TFrame3b }

function TFrame3b.GetBattery: extended;
begin
  result:=_Battery*0.1;
end;

function TFrame3b.GetCircAirMotorCurrent: extended;
begin
  result:=_CircAirMotorCurrent/10.0;
end;

function TFrame3b.GetExtractorFanRpm: integer;
begin
  if _ExtractorFanRpm=0 then
  begin
    result:=0;
    exit;
  end;
  result:=Round(58594.0 / _ExtractorFanRpm);
  if result<260 then
    result:=0;
end;

function TFrame3b.GetPumpSafetySwitch: string;
begin
  if CurrentErrorHydronic=48 then
    result:='open'
  else
    result:='closed';
end;

{ TFrame35 }

function TFrame35.GetBurnerFanVoltage: extended;
begin
  result:=_BurnerFanVoltage*0.1;
end;

function TFrame35.GetBurnerStatus: string;
begin
  case _AV3_Hydronic and $2f of
       0: result:='no info';
       1: result:='blow cold';
       2: result:='level high';
       4: result:='level middle';
       8: result:='level low';
       $10: result:='system check';
       $20: result:='run after';
       else result:='?'+inttohex(_AV3_Hydronic,2)
  end;
end;

function TFrame35.GetHydronicFlame: string;
begin
  if _EVENT0_Hydronic and $10 = 0 then
    result:='off'
  else
    result:='on';
end;

function TFrame35.GetHydronicState: string;
begin
  if _EVENT0_Hydronic and 2 =0  then
    result:='off'
  else
    result:='on'
end;

function TFrame35.GetGlowPlugStatus: string;
begin
  case _AV2_Hydronic and $0c of
       0: result:='no voltage';
       4: result:='start';
       8: result:='restart';
       else result:='?'+inttohex(_AV2_Hydronic and $0c,1);
  end;

end;

{ TFrame39 }

function TFrame39.GetBlowOutTemperature: extended;
begin
  result:=LeToN(_BlowOutTemperature)*0.1;
end;

function TFrame39.GetFlameTemperature: extended;
begin
  //NOTE: up to 290ºC OK, up to 390 warning, over 390 error
  result:=RawToFlameTemperature(_FlameTemperature);
end;

function TFrame39.GetPumpFrequency: extended;
begin
   result:=_PumpFrequency/25.0;
end;

{ TFrame34 }

function TFrame34.GetEbtMode: string;
begin
  case _EbtMode of
     0: result:='Electric 1800W';
     1: result:='Electric 900W';
     2: result:='Gas/Diesel';
     3: result:='Mixed 900W';
     4: result:='Mixed 1800W';
     else result:='?'+inttostr(_EbtMode);
  end;
end;

function TFrame34.GetHydronicStartInfo: string;
var
  b: Byte;
begin
  b:=_Event2 and $7;
  case b of
     0:result:='No start info';
     1:result:='1st start';
     2:result:='Norm. start';
     4:result:='Repeat start'
     else result:='?'+inttohex(b,1);
  end;
end;

function TFrame34.GetOperationTime: integer;
begin
  result:=_OperationTime[0]*65536+_OperationTime[1]*256+_OperationTime[2];
end;

{ TFrame14 }

function TFrame14.GetRoomTargetTemperature: extended;
begin
  result:=GetTemperature(_RoomTargetTemperature);
end;

function TFrame14.GetRoomTemperature: extended;
begin
  result:=GetTemperature(_RoomTemperature);
end;

function TFrame14.GetWaterTargetTemperature: extended;
begin
  result:=GetTemperature(_WaterTargetTemperature);
end;

{ TFrame16 }

function TFrame16.GetBatteryVoltage: extended;
begin
  result:=LeToN(_BatteryVoltage)*0.01-327.67
end;

function TFrame16.GetRoomTemperature: extended;
begin
  result:=GetTemperature(_RoomTemperature);
end;

function TFrame16.GetWaterTemperature: extended;
begin
  result:=GetTemperature(_WaterTemperature);
end;

end.

