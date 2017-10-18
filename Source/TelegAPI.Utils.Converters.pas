unit TelegAPI.Utils.Converters;

interface

uses
  System.Rtti,
  JsonDataObjects,
  DJSON.Serializers;

type
  TConverterEnums<T> = class(TdjJDOCustomSerializer)
  public
    class procedure Serialize(const AJSONValue: PJsonDataValue; const AValue: TValue); override;
    class function Deserialize(const AJSONValue: PJsonDataValue; const AExistingValue: TValue): TValue; override;
    class function isTypeNotificationCompatible: Boolean; override;
  end;

  TConverterDate = class(TdjJDOCustomSerializer)
  public
    class procedure Serialize(const AJSONValue: PJsonDataValue; const AValue: TValue); override;
    class function Deserialize(const AJSONValue: PJsonDataValue; const AExistingValue: TValue): TValue; override;
    class function isTypeNotificationCompatible: Boolean; override;
  end;

implementation

uses
  System.DateUtils,
  TelegAPI.Utils;

{ TConverterTgTypeChat<T> }

class function TConverterEnums<T>.Deserialize(const AJSONValue: PJsonDataValue; const AExistingValue: TValue): TValue;
var
  LEnumConv: TEnumConverter<T>;
begin
  LEnumConv := TEnumConverter<T>.Create;
  try
    Result := TValue.From<T>(LEnumConv.FromString(AJSONValue.Value));
  finally
    LEnumConv.Free;
  end;
end;

class function TConverterEnums<T>.isTypeNotificationCompatible: Boolean;
begin
  Result := True;
end;

class procedure TConverterEnums<T>.Serialize(const AJSONValue: PJsonDataValue; const AValue: TValue);
var
  LEnumConv: TEnumConverter<T>;
begin
  inherited;
  LEnumConv := TEnumConverter<T>.Create;
  try
    AJSONValue.Value := LEnumConv.ToString(AValue.AsType<T>);
  finally
    LEnumConv.Free;
  end;
end;

{ TConverterDate }

class function TConverterDate.Deserialize(const AJSONValue: PJsonDataValue; const AExistingValue: TValue): TValue;
begin
  if AJSONValue <> nil then
    Result := TValue.From<TDateTime>(UnixToDateTime(AJSONValue.LongValue, False));
end;

class function TConverterDate.isTypeNotificationCompatible: Boolean;
begin
  Result := True;
end;

class procedure TConverterDate.Serialize(const AJSONValue: PJsonDataValue; const AValue: TValue);
begin
  inherited;
  AJSONValue.LongValue := DateTimeToUnix(AValue.AsType<TDateTime>, False);
end;

end.

