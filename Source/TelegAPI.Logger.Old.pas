unit TelegAPI.Logger.Old;

interface

uses
  CloudAPI.Logger,
  System.SysUtils;

type
  TtgExceptionManagerConsole = class(TLogEmpty)
  private
    FOnLog: TProc<TLogLevel, string, Exception>;
  public
    procedure Log(level: TLogLevel; const msg: string; const e: Exception); override;
    property OnLog: TProc<TLogLevel, string, Exception> read FOnLog write FOnLog;
  end;

  TtgOnLog = procedure(ASender: TObject; const Level: TLogLevel; const Msg:
    string; E: Exception) of object;

  TtgExceptionManagerUI = class(TLogEmpty)
  private
    FOnLog: TtgOnLog;
  public
    procedure Log(level: TLogLevel; const msg: string; const e: Exception); override;
  published
    property OnLog: TtgOnLog read FOnLog write FOnLog;
  end;

implementation

{ TtgExceptionManagerConsole }

procedure TtgExceptionManagerConsole.Log(level: TLogLevel; const msg: string;
  const e: Exception);
begin
  inherited;
  if Assigned(OnLog) then
    OnLog(level, msg, e)
  else if level >= TLogLevel.Error then
    raise e;
end;

{ TtgExceptionManagerUI }

procedure TtgExceptionManagerUI.Log(level: TLogLevel; const msg: string; const e:
  Exception);
begin
  inherited;
  if Assigned(OnLog) then
    OnLog(Self, level, msg, e)
  else if level >= TLogLevel.Error then
    raise e;
end;

end.

