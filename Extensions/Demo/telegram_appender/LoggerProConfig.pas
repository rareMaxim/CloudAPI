unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender,
  LoggerPro.TelegramAppender,
  LoggerPro.OutputDebugStringAppender,
  System.SysUtils, System.IOUtils;

var
  _Log: ILogWriter;


function Log: ILogWriter;
begin
  Result := _Log;
end;

procedure SetupLogger;
const
{$IFDEF DEBUG}
  LOG_LEVEL = TLogType.Debug;
{$ELSE}
  LOG_LEVEL = TLogType.Warning;
{$ENDIF}
var
  lTelegramAppender: ILogAppender;
begin
  lTelegramAppender := TLoggerProTelegramAppender.Create(   //
  {}  paste your token,{} //
    'Your mashine ID',               //
    '245903278');         // ID for recesive reports
  lTelegramAppender.SetLogLevel(TLogType.Error);
  _Log := BuildLogWriter(
  [TLoggerProFileAppender.Create,
  lTelegramAppender,
  TLoggerProOutputDebugStringAppender.Create], nil, LOG_LEVEL);
end;

initialization
  SetupLogger;

end.

