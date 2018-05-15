unit TelegAPI.Logger.Old;

interface

uses
  TelegAPI.Logger,
  System.SysUtils;

type
  TtgExceptionManagerConsole = class(TLogAbstract)
  private
    FOnLog: TProc<TLogLevel, string, Exception>;
  public
    procedure Log(level: TLogLevel; const msg: string; const e: Exception); override;
    property OnLog: TProc<TLogLevel, string, Exception> read FOnLog write FOnLog;
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

end.

