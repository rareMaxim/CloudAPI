unit CloudAPI.RequestArgument;

interface

uses
  CloudAPI.Request,
  CloudAPI.Parameter,
  CloudAPI.Types,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections;

type

  TcaTypeConverter = class(TDictionary < string, TFunc < TValue, string >> )
  end;

  TcaRequestArgument = class
  private
    class var fCurrent: TcaRequestArgument;
  private
    fConverter: TcaTypeConverter;
    fRtti: TRttiContext;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterConverter<T>(AConverter: TFunc<TValue, string>);
    procedure RegisterToJson<T>;
    function ObjToParams(AArguments: Pointer; AType: TRttiType; ADefaultParam: TcaParameter)
      : TArray<TcaParameter>; overload;
    function ObjToParams<T>(AArguments: T): TArray<TcaParameter>; overload;
    function ObjToRequest<T>(AArguments: T): IcaRequest; overload;
    function ConvertToString(AValue: TValue): string;
    function TryConvertToString(AValue: TValue; var AStringValue: string): Boolean;
    function TryGetConverterName(AValue: TValue; var AConverterName: string): Boolean;
    function ParsePrototype(AType: Pointer; var ARttiType: TRttiType; var ADefaltParam: TcaParameter;
      var Resourse: string; var AMethod: TcaMethod): Boolean;
    function ParseLimitInfo(ARttiType: TRttiType; AResourse: string; ALimitInfo: TcaRequestLimit): Boolean;

    class function Current: TcaRequestArgument;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  CloudAPI.Attributes,
  CloudAPI.Converter.BasicTypes, CloudAPI.Client.Base;

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

function TcaRequestArgument.TryConvertToString(AValue: TValue; var AStringValue: string): Boolean;
var
  LName: string;
begin
  Result := TryGetConverterName(AValue, LName);
  if Result then
    AStringValue := fConverter[LName](AValue)
end;

function TcaRequestArgument.TryGetConverterName(AValue: TValue; var AConverterName: string): Boolean;
begin
  if AValue.IsEmpty then
  begin
    AConverterName := 'AValue.IsEmpty';
    Exit(False);
  end;
  AConverterName := GetShortStringString(@AValue.TypeInfo.Name);
  Result := fConverter.ContainsKey(AConverterName);
end;

function TcaRequestArgument.ConvertToString(AValue: TValue): string;
begin
  if not TryConvertToString(AValue, Result) then
    raise ENotSupportedException.CreateFmt('Converter for "%S" not supported', [AValue.ToString]);
end;

class constructor TcaRequestArgument.Create;
begin
  fCurrent := TcaRequestArgument.Create;
end;

class destructor TcaRequestArgument.Destroy;
begin
  fCurrent.Free;
end;

function TcaRequestArgument.ObjToParams(AArguments: Pointer; AType: TRttiType; ADefaultParam: TcaParameter)
  : TArray<TcaParameter>;
var
  LRttiField: TRttiField;
  LRttiAttr: TCustomAttribute;
  LParam: TcaParameter;
  lParamList: TList<TcaParameter>;
  LArguments: Pointer;
  lIsCaParameter: Boolean;
begin
  if AType.TypeKind = TTypeKind.tkClass then // <------Viktor Akselrod
    LArguments := PPointer(AArguments)^
  else
    LArguments := AArguments;
  if not Assigned(LArguments) then
    Exit;
  lParamList := TList<TcaParameter>.Create;
  try
    for LRttiField in AType.GetFields do
    begin
      lIsCaParameter := False;
      LParam := ADefaultParam;
      LParam.IsRequired := False;
      LParam.Name := LRttiField.Name;
      LParam.Value := LRttiField.GetValue(LArguments);
      for LRttiAttr in LRttiField.GetAttributes do
      begin
        if LRttiAttr is TcaCustomAttribute then
          lIsCaParameter := True; // Поле является параметром для CloudAPI
        if LRttiAttr is caIsRequairedAttribute then
          LParam.IsRequired := (LRttiAttr as caIsRequairedAttribute).IsRequired
        else if LRttiAttr is caNameAttribute then
          LParam.Name := (LRttiAttr as caNameAttribute).Name
        else if LRttiAttr is caDefaultValueAttribute then
          LParam.DefaultValue := (LRttiAttr as caDefaultValueAttribute).ToString
        else if LRttiAttr is caParameterTypeAttribute then
          LParam.ParameterType := (LRttiAttr as caParameterTypeAttribute).ParameterType;
      end;
      if lIsCaParameter then
        lParamList.Add(LParam);
    end;
    Result := lParamList.ToArray;
  finally
    lParamList.Free;
  end;
end;

function TcaRequestArgument.ObjToRequest<T>(AArguments: T): IcaRequest;
var
  LRttiType: TRttiType;
  LParam: TcaParameter;
  lParams: TArray<TcaParameter>;
  lRes: string;
  lMethod: TcaMethod;
begin
  // Result := ObjToRequest(@AArguments, TypeInfo(T));
  Result := TcaRequest.Create;
  // ParsePrototype(AType, LRttiType, LParam, lRes, lMethod);
  ParsePrototype(TypeInfo(T), LRttiType, LParam, lRes, lMethod);
  Result.Resource := lRes;
  Result.Method := lMethod;
  ParseLimitInfo(LRttiType, Result.Resource, Result.LimitInfo);
  // lParams := ObjToParams(AArguments, LRttiType, LParam);
  lParams := ObjToParams(@AArguments, LRttiType, LParam);
  for LParam in lParams do
    Result.AddParam(LParam);
end;

function TcaRequestArgument.ParseLimitInfo(ARttiType: TRttiType; AResourse: string;
  ALimitInfo: TcaRequestLimit): Boolean;
var
  LRttiAttr: TCustomAttribute;
begin
  Result := True;
  for LRttiAttr in ARttiType.GetAttributes do
  begin
    if LRttiAttr is caLimitedMethodAttribute then
    begin
      ALimitInfo := TcaRequestLimit.Create( //
        (LRttiAttr as caLimitedMethodAttribute).Limit, //
        AResourse, //
        (LRttiAttr as caLimitedMethodAttribute).IsGlobal)
    end;
  end;
end;

function TcaRequestArgument.ParsePrototype(AType: Pointer; var ARttiType: TRttiType; var ADefaltParam: TcaParameter;
  var Resourse: string; var AMethod: TcaMethod): Boolean;
var
  LRttiAttr: TCustomAttribute;
begin
  Result := True;
  ADefaltParam.ParameterType := TcaParameterType.QueryString;
  AMethod := TcaMethod.GET;
  ARttiType := fRtti.GetType(AType);
  for LRttiAttr in ARttiType.GetAttributes do
  begin
    if LRttiAttr is caNameAttribute then
      Resourse := (LRttiAttr as caNameAttribute).Name;
    if LRttiAttr is caMethodAttribute then
      AMethod := (LRttiAttr as caMethodAttribute).Method;
    if LRttiAttr is caParameterTypeAttribute then
      ADefaltParam.ParameterType := (LRttiAttr as caParameterTypeAttribute).ParameterType;
  end;
end;

procedure TcaRequestArgument.RegisterConverter<T>(AConverter: TFunc<TValue, string>);
var
  LTypeInfo: PTypeInfo;
  LName: string;
begin
  LTypeInfo := TypeInfo(T);
  LName := string(LTypeInfo.Name);
  fConverter.AddOrSetValue(LName, AConverter);
end;

procedure TcaRequestArgument.RegisterToJson<T>;
begin
  RegisterConverter<T>(
    function(AValue: TValue): string
    var
      lData: T;
      lCA: TCloudApiClientBase;
    begin
      lData := AValue.AsType<T>;
      lCA := TCloudApiClientBase.Create;
      try
        Result := lCA.Serializer.Serialize<T>(lData);
      finally
        lCA.Free;
      end;
    end);
end;

constructor TcaRequestArgument.Create;
begin
  fConverter := TcaTypeConverter.Create();
  fRtti := TRttiContext.Create();
  TcaBasicConverters.BasicConverter(Self);
end;

class function TcaRequestArgument.Current: TcaRequestArgument;
begin
  Result := fCurrent;
end;

destructor TcaRequestArgument.Destroy;
begin
  fConverter.Free;
  fRtti.Free;
end;

function TcaRequestArgument.ObjToParams<T>(AArguments: T): TArray<TcaParameter>;
var
  LRttiType: TRttiType;
  lDefaultParameter: TcaParameter;
  lRes: string;
  lMethod: TcaMethod;
begin
  ParsePrototype(TypeInfo(T), LRttiType, lDefaultParameter, lRes, lMethod);
  Result := ObjToParams(@AArguments, LRttiType, lDefaultParameter);
end;

end.
