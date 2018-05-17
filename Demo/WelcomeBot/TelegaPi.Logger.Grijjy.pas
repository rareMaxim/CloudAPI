unit TelegaPi.Logger.Grijjy;

interface

uses
  Grijjy.CloudLogging,
  TelegaPi.Logger,
  System.SysUtils;

type
  TtgGrijjyLogger = class(TLogEmpty)
  protected
    function LogLvlToGLogLvl(Lvl: TLogLevel): TgoLogLevel;
  public
    procedure Enter(const instance: TObject; const MethodName: string); override;
    procedure Leave(const instance: TObject; const MethodName: string); override;
    procedure Log(level: TLogLevel; const msg: string; const e: Exception); override;
  end;

implementation

uses
  System.TypInfo;


{ TtgGrijjyLogger }

procedure TtgGrijjyLogger.Enter(const instance: TObject; const methodName: string);
begin
  inherited;
  GrijjyLog.EnterMethod(instance, methodName);
end;

procedure TtgGrijjyLogger.Leave(const instance: TObject; const methodName: string);
begin
  inherited;
  GrijjyLog.ExitMethod(instance, methodName);
end;

procedure TtgGrijjyLogger.Log(level: TLogLevel; const msg: string; const e: Exception);
var
  LMsg: string;
begin
  inherited;
  LMsg := msg;
  if Assigned(e) then
    LMsg := LMsg + ': ' + e.ToString;
  GrijjyLog.Send(LMsg, LogLvlToGLogLvl(level));
end;

function TtgGrijjyLogger.LogLvlToGLogLvl(Lvl: TLogLevel): TgoLogLevel;
begin
  case Lvl of
    Warn:
      Result := TgoLogLevel.Warning;
    TLogLevel.Error, TLogLevel.Fatal:
      Result := TgoLogLevel.Error;
  else
    Result := TgoLogLevel.Info;
  end
end;

end.

