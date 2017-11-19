unit TelegAPI.Utils;

interface

type
  TEnumConverter<T> = class
  public
    function ToString(EnumValue: T): string; reintroduce;
    function ToInt(EnumValue: T): Integer;
    function FromString(const strValue: string): T;
  end;

  TIntfC = class
    class function Test<T, I>: I;
  end;

implementation

uses
  System.TypInfo;

{ TEnumConverterg<T> }

function TEnumConverter<T>.FromString(const strValue: string): T;
var
  LType: PTypeInfo;
  Temp: Integer;
  PTemp: Pointer;
begin
  LType := TypeInfo(T);
  Temp := GetEnumValue(LType, strValue);
  PTemp := @Temp;
  Result := T(PTemp^);
end;

function TEnumConverter<T>.ToInt(EnumValue: T): Integer;
begin
  Result := 0;
  Move(EnumValue, Result, SizeOf(EnumValue));
end;

function TEnumConverter<T>.ToString(EnumValue: T): string;
begin
  Result := GetEnumName(TypeInfo(T), ToInt(EnumValue));
end;

{ TIntfC }

class function TIntfC.Test<T, I>: I;
begin

end;

end.

