unit TelegAPI.Utils.Json;

interface

type
  TJsonUtils = class
    class function ArrayToJString<T>(LArray: TArray<T>): string;
  end;

implementation

uses
  DJSON,
  JsonDataObjects,
  System.Rtti;
{ TJsonUtils }

class function TJsonUtils.ArrayToJString<T>(LArray: TArray<T>): string;
var
  JDOArray: TJsonArray;
  LTasString: string;
  I: Integer;
begin
  JDOArray := TJsonArray.Create;
  try
    for I := Low(LArray) to High(LArray) do
    begin
      LTasString := dj.From(TValue.From<T>(LArray[I]), dj.Default).ToJson;
      JDOArray.Add(LTasString);
    end;
    Result := JDOArray.ToJSON();
  finally
    JDOArray.free;
  end;
end;

end.

