unit CloudAPI.RequestArgument;

interface

uses
  CloudAPI.Request,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections;

type

  TcaTypeConverter = class(TDictionary < string, TFunc < TValue, string >> )
  end;

  TcaRequestArgument = class
  private
    class var FConverter: TcaTypeConverter;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter<T>(AConverter: TFunc<TValue, string>);
    class function ObjToRequest<T>(AArguments: T): IcaRequest; overload;
    class function ObjToRequest(AArguments: Pointer; AType: Pointer): IcaRequest; overload;
    class function ConvertToString(AValue: TValue): string;
  end;

implementation

uses
  CloudAPI.Attributes,
  CloudAPI.Converter.BasicTypes,
  CloudAPI.Parameter,
  CloudAPI.Types;

function GetShortStringString(const ShortStringPointer: PByte): string;
var
  ShortStringLength: Byte;
  FirstShortStringCharacter: MarshaledAString;
  ConvertedLength: Cardinal;
  UnicodeCharacters: array [Byte] of Char;
  // cannot be more than 255 characters, reserve 1 character for terminating null
begin
  if not Assigned(ShortStringPointer) then
    Result := ''
  else
  begin
    ShortStringLength := ShortStringPointer^;
    if ShortStringLength = 0 then
      Result := ''
    else
    begin
      FirstShortStringCharacter := MarshaledAString(ShortStringPointer + 1);
      ConvertedLength := UTF8ToUnicode(UnicodeCharacters, Length(UnicodeCharacters), FirstShortStringCharacter,
        ShortStringLength);
      // UTF8ToUnicode will always include the null terminator character in the Result:
      ConvertedLength := ConvertedLength - 1;
      SetString(Result, UnicodeCharacters, ConvertedLength);
    end;
  end;
end;
{ TcaRequestArgument }

class function TcaRequestArgument.ConvertToString(AValue: TValue): string;
var
  LName: string;
begin
  if AValue.IsEmpty then
    Exit('');
  LName := GetShortStringString(@AValue.TypeInfo.Name);
  if not FConverter.ContainsKey(LName) then
    raise ENotSupportedException.CreateFmt('Converter for %s not supported', [AValue.TypeInfo.Name]);
  Result := FConverter[LName](AValue);
end;

class constructor TcaRequestArgument.Create;
begin
  FConverter := TcaTypeConverter.Create();
  TcaBasicConverters.BasicConverter;
end;

class destructor TcaRequestArgument.Destroy;
begin
  FConverter.Free;
end;

class function TcaRequestArgument.ObjToRequest(AArguments: Pointer; AType: Pointer): IcaRequest;
var
  LRtti: TRttiContext;
  LRttiType: TRttiType;
  LRttiField: TRttiField;
  LRttiAttr: TCustomAttribute;
  LParam: TcaParameter;
begin
  Result := TcaRequest.Create;
  LRtti := TRttiContext.Create();
  try
    LParam.ParameterType := TcaParameterType.QueryString;
    LRttiType := LRtti.GetType(AType);
    for LRttiAttr in LRttiType.GetAttributes do
    begin
      if LRttiAttr is caNameAttribute then
        Result.Resource := (LRttiAttr as caNameAttribute).Name;
      if LRttiAttr is caMethodAttribute then
        Result.Method := (LRttiAttr as caMethodAttribute).Method;
      if LRttiAttr is caParameterTypeAttribute then
        LParam.ParameterType := (LRttiAttr as caParameterTypeAttribute).ParameterType;
    end;

    for LRttiAttr in LRttiType.GetAttributes do
    begin
      if LRttiAttr is caLimitedMethodAttribute then
      begin
        Result.LimitInfo := TcaRequestLimit.Create( //
          (LRttiAttr as caLimitedMethodAttribute).Limit, //
          Result.Resource, //
          (LRttiAttr as caLimitedMethodAttribute).IsGlobal)
      end;
    end;
    for LRttiField in LRttiType.GetFields do
    begin
      LParam.IsRequired := False;
      LParam.Name := LRttiField.Name;
      LParam.Value := LRttiField.GetValue(AArguments);
      for LRttiAttr in LRttiField.GetAttributes do
      begin
        if LRttiAttr is caIsRequairedAttribute then
          LParam.IsRequired := (LRttiAttr as caIsRequairedAttribute).IsRequired
        else if LRttiAttr is caNameAttribute then
          LParam.Name := (LRttiAttr as caNameAttribute).Name
        else if LRttiAttr is caDefaultValueAttribute then
          LParam.DefaultValue := (LRttiAttr as caDefaultValueAttribute).ToString
        else if LRttiAttr is caParameterTypeAttribute then
          LParam.ParameterType := (LRttiAttr as caParameterTypeAttribute).ParameterType;
      end;
      Result.AddParam(LParam);
    end;
  finally
    LRtti.Free;
  end;
end;

class function TcaRequestArgument.ObjToRequest<T>(AArguments: T): IcaRequest;
begin
  Result := ObjToRequest(@AArguments, TypeInfo(T));
end;

class procedure TcaRequestArgument.RegisterConverter<T>(AConverter: TFunc<TValue, string>);
var
  LTypeInfo: PTypeInfo;
  LName: string;
begin
  LTypeInfo := TypeInfo(T);
  LName := string(LTypeInfo.Name);
  FConverter.AddOrSetValue(LName, AConverter);
end;

end.
