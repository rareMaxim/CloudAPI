unit WelcomeBot.Logger;

interface

function Log(LogStr: string; Prefix: string; IsError: Boolean = False): string;

implementation

uses
  System.SysUtils;

function Log(LogStr: string; Prefix: string; IsError: Boolean): string;
var
  _Prefix, _Error, S: WideString;
  F: TextFile;
begin
  AssignFile(F, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    ExtractFileName(ParamStr(0)) + '.log');
  try
    if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      ExtractFileName(ParamStr(0)) + '.log') then
      Append(F)
    else
      Rewrite(F);
    if Trim(Prefix) <> '' then
      _Prefix := '[' + Prefix + '] '
    else
      _Prefix := '';
    if IsError then
      _Error := '! '
    else
      _Error := '';
    S := _Prefix + LogStr;
    Result := S;
    WriteLn(F, _Error + DateTimeToStr(Now) + ' ' + S);
  except
    // on E: Exception do
    // LogMessage(S + #13#10 + E.Message, EVENTLOG_ERROR_TYPE);
  end;
  CloseFile(F);
end;

end.
