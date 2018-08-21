unit CloudAPI.Utils.Json;
{ /$I jedi\jedi.inc }

interface

uses
  System.Generics.Collections,
  System.Json;

type
  TBaseJsonClass = class of TBaseJson;

  TBaseJson = class(TInterfacedObject)
  private
    FJSON: TJSONObject;
    FJsonRaw: string; // for debbuger
  protected
    function GetJson: TJSONObject;
  public
    function ToClass<T: class, constructor>(const AKey: string): T;
    function ToSimpleType<T>(const AKey: string): T;
    function ToDateTime(const AKey: string): TDateTime;
    function ToSimpleArray<T>(const AKey: string): TArray<T>;
    function ToArray<TI: IInterface>(TgClass: TBaseJsonClass; const AKey: string): TArray<TI>;
    function ToPairs(const AKey: string): TArray<TPair<string, string>>; overload;
    function ToPairs<TI: IInterface>(TgClass: TBaseJsonClass; const AKey: string): TArray<TPair<string, TI>>; overload;
    function ToPairsAsArray<TI: IInterface>(TgClass: TBaseJsonClass; const AKey: string): TArray<TI>;
    procedure Write(const AKey, AValue: string); overload;
    procedure Write(const AKey: string; AValue: TJSONValue); overload;
    procedure SetJson(const AJson: string);
    function AsJson: string;
    function AsBoolean: Boolean;
    function AsString: string;
    class function AsArray<TI>(const TgClass: TBaseJsonClass; const AValue: string): TArray<TI>;
    class function AsClass<T: TBaseJson, constructor>(const AJson: string): T;
    class function AsJSONArray(const AValue: string): TJSONArray;
    class function FromJson(const AJson: string): TBaseJson;
    class function GetTgClass: TBaseJsonClass; virtual; // abstract;
    class procedure UnSupported;
    constructor Create(const AJson: string = '{}'); virtual;
    destructor Destroy; override;
  end;

  TJsonUtils = class
    class function ArrayToJString<T: class>(LArray: TArray<T>): string;
    class function ObjectToJString(AObj: TObject): string;
    class function FileToObject<T: class, constructor>(const AFileName: string): T;
    class procedure ObjectToFile(AObj: TObject; const AFileName: string);
  end;

implementation

uses
  REST.Json,
  System.DateUtils,
  System.IOUtils,
  System.SysUtils,
  System.TypInfo;

type
  TRCStrings = class
  public const
    BAD_INTF_FOR_CLASS = 'Unsupported interface for %S';
  end;

  { TJsonUtils }

class function TJsonUtils.ArrayToJString<T>(LArray: TArray<T>): string;
var
  I: Integer;
begin
  Result := '[';
  for I := Low(LArray) to High(LArray) do
    if Assigned(LArray[I]) then
    begin
      Result := Result + TJson.ObjectToJsonString(LArray[I]);
      if I <> High(LArray) then
        Result := Result + ',';
    end;
  Result := Result + ']';
end;

class function TJsonUtils.FileToObject<T>(const AFileName: string): T;
var
  LContent: string;
begin
  Result := nil;
  if TFile.Exists(AFileName) then
  begin
    LContent := TFile.ReadAllText(AFileName, TEncoding.UTF8);
    Result := TJson.JsonToObject<T>(LContent);
  end;
end;

class procedure TJsonUtils.ObjectToFile(AObj: TObject; const AFileName: string);
var
  LContent: string;
begin
  LContent := ObjectToJString(AObj);
  TFile.WriteAllText(AFileName, LContent, TEncoding.UTF8);
end;

class function TJsonUtils.ObjectToJString(AObj: TObject): string;
begin
{$IF Defined(DELPHIX_TOKYO_UP)}
  Result := TJson.ObjectToJsonString(AObj);
{$ELSE}
  if Assigned(AObj) then
    Result := TJson.ObjectToJsonString(AObj)
  else
    Result := 'null';
{$ENDIF}
end;

{ TBaseJson }
function TBaseJson.ToArray<TI>(TgClass: TBaseJsonClass; const AKey: string): TArray<TI>;
var
  LTmpArray: TJSONArray;
begin
  Result := nil;
  LTmpArray := FJSON.GetValue(AKey) as TJSONArray;
  if Assigned(LTmpArray) then
    Result := TBaseJson.AsArray<TI>(TgClass, LTmpArray.ToJSON);
end;

class function TBaseJson.AsArray<TI>(const TgClass: TBaseJsonClass; const AValue: string): TArray<TI>;
var
  LJsonArr: TJSONArray;
  I: Integer;
  GUID: TGUID;
begin
  GUID := GetTypeData(TypeInfo(TI))^.GUID;
  // check for TI interface support
  if TgClass.GetInterfaceEntry(GUID) = nil then
  begin
    raise Exception.Create(Format(TRCStrings.BAD_INTF_FOR_CLASS, [TgClass.ClassName]));
  end;
  // stage 2: proceed data
  LJsonArr := AsJSONArray(AValue);
  if (not Assigned(LJsonArr)) or LJsonArr.Null then
    Exit(nil);
  try
    SetLength(Result, LJsonArr.Count);
    for I := 0 to High(Result) do
      TgClass.GetTgClass.Create(LJsonArr.Items[I].ToString).GetInterface(GUID, Result[I]);
  finally
    LJsonArr.Free;
  end;

end;

function TBaseJson.AsBoolean: Boolean;
begin
  Result := (FJSON as TJSONValue) is TJSONTrue;
end;

class function TBaseJson.AsClass<T>(const AJson: string): T;
var
  LValue: string;
  FJSON: TJSONObject;
begin
  Result := nil;
  FJSON := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  if Assigned(FJSON) and (not FJSON.Null) then
  begin
    Result := TBaseJsonClass(T).Create(AJson) as T;
  end
end;

function TBaseJson.AsJson: string;
begin
  if Assigned(FJSON) then
    Result := FJSON.ToJSON
  else
    Result := '';
end;

class function TBaseJson.AsJSONArray(const AValue: string): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(AValue) as TJSONArray;
end;

function TBaseJson.AsString: string;
begin
  Result := FJSON.Value;
end;

constructor TBaseJson.Create(const AJson: string);
begin
  inherited Create;
  SetJson(AJson);
  if FJSON.Null then
    Self := nil;
end;

function TBaseJson.ToClass<T>(const AKey: string): T;
var
  LValue: string;
  LObj: TJSONValue;
begin
  Result := nil;
  LObj := FJSON.GetValue(AKey);
  if Assigned(LObj) and (not LObj.Null) then
  begin
    LValue := LObj.ToJSON;
    Result := TBaseJsonClass(T).Create(LValue) as T;
  end
end;

destructor TBaseJson.Destroy;
begin
  FJSON.Free;
  inherited;
end;

class function TBaseJson.FromJson(const AJson: string): TBaseJson;
begin
  if AJson.IsEmpty then
    Result := nil
  else
    Result := TBaseJson.Create(AJson);
end;

function TBaseJson.GetJson: TJSONObject;
begin
  Result := FJSON;
end;

class function TBaseJson.GetTgClass: TBaseJsonClass;
begin
  Result := Self;
end;

function TBaseJson.ToDateTime(const AKey: string): TDateTime;
var
  LValue: Int64;
begin
  Result := 0;
  if FJSON.TryGetValue<Int64>(AKey, LValue) then
    Result := UnixToDateTime(LValue, False);
end;

function TBaseJson.ToSimpleArray<T>(const AKey: string): TArray<T>;
var
  LJsonArray: TJSONArray;
  I: Integer;
begin
  LJsonArray := FJSON.GetValue(AKey) as TJSONArray;
  if (not Assigned(LJsonArray)) or LJsonArray.Null then
    Exit(nil);
  SetLength(Result, LJsonArray.Count);
  for I := 0 to High(Result) do
  begin
    if (not Assigned(LJsonArray.Items[I])) or (not LJsonArray.Items[I].TryGetValue<T>(Result[I])) then
      Result[I] := Default (T);
  end;
end;

function TBaseJson.ToSimpleType<T>(const AKey: string): T;
begin
  if (not Assigned(FJSON)) or (not FJSON.TryGetValue<T>(AKey, Result)) then
    Result := Default (T);
end;

procedure TBaseJson.SetJson(const AJson: string);
begin
  FJsonRaw := AJson;
  if FJsonRaw.IsEmpty then
  begin
    Exit;
  end;
  if Assigned(FJSON) then
    FreeAndNil(FJSON);
  FJSON := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
end;

function TBaseJson.ToPairsAsArray<TI>(TgClass: TBaseJsonClass; const AKey: string): TArray<TI>;
var
  LTemp: TArray<TPair<string, TI>>;
  I: Integer;
begin
  LTemp := ToPairs<TI>(TgClass, AKey);
  SetLength(Result, Length(LTemp));
  for I := Low(Result) to High(Result) do
    Result[I] := LTemp[I].Value;
end;

function TBaseJson.ToPairs(const AKey: string): TArray<TPair<string, string>>;
var
  LJPair: TJSONPair;
  LProducts: TJSONValue;
  LProduct: TJSONValue;
  LIndex: Integer;
begin
  LProducts := FJSON.Get(AKey).JsonValue;
  SetLength(Result, TJSONArray(LProducts).Count);
  for LIndex := 0 to Length(Result) - 1 do
  begin
    LProduct := TJSONArray(LProducts).Items[LIndex];
    LJPair := TJSONPair(LProduct);
    Result[LIndex] := TPair<string, string>.Create(LJPair.JsonString.Value, (LJPair.JsonValue.ToJSON));
  end;
end;

function TBaseJson.ToPairs<TI>(TgClass: TBaseJsonClass; const AKey: string): TArray<TPair<string, TI>>;
var
  LItem: TPair<string, string>;
  FList: TList<TPair<string, TI>>;
  GUID: TGUID;
  LValue: TI;
begin
  FList := TList < TPair < string, TI >>.Create;
  try
    for LItem in ToPairs(AKey) do
    begin
      GUID := GetTypeData(TypeInfo(TI))^.GUID;
      if TgClass.GetInterfaceEntry(GUID) = nil then
        raise Exception.Create(Format(TRCStrings.BAD_INTF_FOR_CLASS, [TgClass.ClassName]));
      TgClass.GetTgClass.Create(LItem.Value).GetInterface(GUID, LValue);
      FList.Add(TPair<string, TI>.Create(LItem.Key, LValue));
    end;
    Result := FList.ToArray;
  finally
    FList.Free;
  end;
end;

class procedure TBaseJson.UnSupported;
begin
  raise Exception.Create('Telegram method not supported in TelegaPi Library. Sorry.');
end;

procedure TBaseJson.Write(const AKey: string; AValue: TJSONValue);
var
  I: Integer;
begin
  for I := 0 to FJSON.Count - 1 do
    if FJSON.Pairs[I].JsonString.Value = AKey then
    begin
      FJSON.Pairs[I].JsonValue := AValue;
      FJsonRaw := FJSON.ToJSON;
      Exit;
    end;
  FJSON.AddPair(AKey, AValue);
  FJsonRaw := FJSON.ToJSON;
end;

procedure TBaseJson.Write(const AKey, AValue: string);
begin
  Write(AKey, TJSONString.Create(AValue));
end;

end.
