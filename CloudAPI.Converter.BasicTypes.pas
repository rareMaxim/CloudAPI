unit CloudAPI.Converter.BasicTypes;

interface

type
  TcaBasicConverters = class
  private
    class procedure StringConverter;
    class procedure Int64Converter;
    class procedure BooleanConverter;
  public
    class procedure BasicConverter;
  end;

implementation

uses
  CloudAPI.RequestArgument,
  System.SysUtils,
  System.Rtti;

{ TcaBasicConverters }

class procedure TcaBasicConverters.BasicConverter;
begin
  StringConverter;
  Int64Converter;
  BooleanConverter;
end;

class procedure TcaBasicConverters.BooleanConverter;
begin
  TcaRequestArgument.RegisterConverter('boolean',
    function(AValue: TValue): string
    begin
      Result := AValue.AsBoolean.ToString(TUseBoolStrs.True);
    end);
end;

class procedure TcaBasicConverters.Int64Converter;
begin
  TcaRequestArgument.RegisterConverter('int64',
    function(AValue: TValue): string
    begin
      Result := AValue.AsInt64.ToString;
    end);
end;

class procedure TcaBasicConverters.StringConverter;
begin
  TcaRequestArgument.RegisterConverter('string',
    function(AValue: TValue): string
    begin
      Result := AValue.AsString;
    end);
end;

end.
