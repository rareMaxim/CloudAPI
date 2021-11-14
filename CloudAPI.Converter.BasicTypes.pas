unit CloudAPI.Converter.BasicTypes;

interface

uses
  CloudAPI.RequestArgument;

type
  TcaBasicConverters = class
  private
    class procedure StringConverter(AConverterManager: TcaRequestArgument);
    class procedure IntegerConverter(AConverterManager: TcaRequestArgument);
    class procedure Int64Converter(AConverterManager: TcaRequestArgument);
    class procedure SingleConverter(AConverterManager: TcaRequestArgument);
    class procedure BooleanConverter(AConverterManager: TcaRequestArgument);
    class procedure TDateTimeConverter(AConverterManager: TcaRequestArgument);
  public
    class procedure BasicConverter(AConverterManager: TcaRequestArgument);
  end;

implementation

uses
  System.DateUtils,
  System.JSON.Serializers,
  System.SysUtils,
  System.Rtti;

{ TcaBasicConverters }

class procedure TcaBasicConverters.BasicConverter(AConverterManager: TcaRequestArgument);
begin
  StringConverter(AConverterManager);
  AConverterManager.RegisterToJson<TArray<string>>;
  IntegerConverter(AConverterManager);
  Int64Converter(AConverterManager);
  BooleanConverter(AConverterManager);
  SingleConverter(AConverterManager);
  TDateTimeConverter(AConverterManager);
end;

class procedure TcaBasicConverters.BooleanConverter(AConverterManager: TcaRequestArgument);
begin
  AConverterManager.RegisterConverter<Boolean>(
    function(AValue: TValue): string
    begin
      Result := AValue.AsBoolean.ToString(TUseBoolStrs.True);
    end);
end;

class procedure TcaBasicConverters.Int64Converter(AConverterManager: TcaRequestArgument);
begin
  AConverterManager.RegisterConverter<Int64>(
    function(AValue: TValue): string
    begin
      Result := AValue.AsInt64.ToString;
    end);
end;

class procedure TcaBasicConverters.IntegerConverter(AConverterManager: TcaRequestArgument);
begin
  AConverterManager.RegisterConverter<Integer>(
    function(AValue: TValue): string
    begin
      Result := AValue.AsInteger.ToString;
    end);
end;

class procedure TcaBasicConverters.SingleConverter(AConverterManager: TcaRequestArgument);
begin
  AConverterManager.RegisterConverter<Single>(
    function(AValue: TValue): string
    var
      FS: TFormatSettings;
    begin
      FS := TFormatSettings.Invariant;
      Result := AValue.AsExtended.ToString(TFloatFormat.ffGeneral, 8, 6, FS);
    end);
end;

class procedure TcaBasicConverters.StringConverter(AConverterManager: TcaRequestArgument);
begin
  AConverterManager.RegisterConverter<string>(
    function(AValue: TValue): string
    begin
      Result := AValue.AsString;
    end);
end;

class procedure TcaBasicConverters.TDateTimeConverter(AConverterManager: TcaRequestArgument);
begin
  AConverterManager.RegisterConverter<TDateTime>(
    function(AValue: TValue): string
    var
      LValue: TDateTime;
    begin
      LValue := AValue.AsType<TDateTime>;
      Result := DateTimeToUnix(LValue).ToString;
    end);
end;

end.
