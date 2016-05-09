unit TelegAPi.Utils;

interface

Type
  TuaUtils = class
    Class Function IfThen<T>(Const Value: Boolean; IfTrue, IfFalse: T): T;
  end;

implementation

{ TuaUtils }

class function TuaUtils.IfThen<T>(const Value: Boolean; IfTrue, IfFalse: T): T;
begin
  if Value then
    Result := IfTrue
  else
    Result := IfFalse;
end;

end.
