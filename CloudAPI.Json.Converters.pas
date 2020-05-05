unit CloudAPI.Json.Converters;

interface

uses
  System.Json.Readers,
  System.Json.Serializers,
  System.Json.Writers,
  System.Rtti,
  System.TypInfo;

type
  // --------------------------------------------------------------------- //
  // Converter for UnixTime
  // --------------------------------------------------------------------- //
  TJsonUnixTimeConverter = class(TJsonConverter)
  public
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue; const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
      const ASerializer: TJsonSerializer): TValue; override;
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
  end;

implementation

uses
  System.DateUtils;
{ TJsonUnixTimeConverter }

function TJsonUnixTimeConverter.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf^.Kind = tkInteger;
end;

function TJsonUnixTimeConverter.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo; const AExistingValue: TValue;
  const ASerializer: TJsonSerializer): TValue;
var
  LEnumValue: TDateTime;
begin
  LEnumValue := UnixToDateTime(AReader.Value.AsInteger());
  TValue.Make(@LEnumValue, ATypeInf, Result);
end;

procedure TJsonUnixTimeConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
  const ASerializer: TJsonSerializer);
begin
  AWriter.WriteValue(DateTimeToUnix(AValue.AsType<TDateTime>(), True));
end;

end.
