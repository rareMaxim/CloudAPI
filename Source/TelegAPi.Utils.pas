unit TelegAPi.Utils;

interface

Type
  TtgUtils = class
    Class Function IfThen<T>(Const Value: Boolean; IfTrue, IfFalse: T): T;
  end;

  TCommandHelper = Class
  private
    FText: TArray<String>;
  public
    Constructor Create(Const Text: String);
    Function IsCommand: Boolean;
    Function Command: String;
    Function ParamCount: Integer;
    Function Param(Const Index: Integer): String;
    Function ParamsToString: String;
  End;

implementation

uses
  System.SysUtils;
{ TuaUtils }

class function TtgUtils.IfThen<T>(const Value: Boolean; IfTrue, IfFalse: T): T;
begin
  if Value then
    Result := IfTrue
  else
    Result := IfFalse;
end;

{ TCommandHelper }

function TCommandHelper.Command: String;
begin
  if Length(FText) = 0 then
    Exit;
  Result := FText[0];
  if Result.Contains('@') then
    Result := Result.Substring(0, FText[0].IndexOf('@'));
end;

constructor TCommandHelper.Create(const Text: String);
begin
  FText := Text.Split([' ', ','], TStringSplitOptions.ExcludeEmpty);
end;

function TCommandHelper.IsCommand: Boolean;
begin
  Result := Length(FText) > 0;
  if Result then
    Result := FText[0].StartsWith('/');
end;

function TCommandHelper.Param(const Index: Integer): String;
begin
  Result := FText[Index];
end;

function TCommandHelper.ParamCount: Integer;
begin
  Result := Length(FText) - 1;
end;

function TCommandHelper.ParamsToString: String;
begin
  Result := string.Join(' ', Copy(FText, 1, Length(FText)));
end;

end.
