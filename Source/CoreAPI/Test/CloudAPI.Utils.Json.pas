unit CloudAPI.Utils.Json;
{/$I jedi\jedi.inc}

interface

uses
  System.Json;

type
  TBaseJsonClass = class of TBaseJson;

  TBaseJson = class(TInterfacedObject)
  private
  protected
    FJSON: TJSONObject;
    FJsonRaw: string; //for debbuger
    function ReadToClass<T: class, constructor>(const AKey: string): T;
    function ReadToSimpleType<T>(const AKey: string): T;
    function ReadToDateTime(const AKey: string): TDateTime;
    function ReadToArray<TI: IInterface>(TgClass: TBaseJsonClass; const AKey:
      string): TArray<TI>;
    procedure Write(const AKey, AValue: string);
    procedure SetJson(const AJson: string);
  public
    function AsJson: string;
    class function FromJson(const AJson: string): TBaseJson;
    class function GetTgClass: TBaseJsonClass; virtual; // abstract;
    class procedure UnSupported;
    constructor Create(const AJson: string); virtual;
    destructor Destroy; override;
  end;

  TJsonUtils = class
    class function ArrayToJString<T: class>(LArray: TArray<T>): string;
    class function ObjectToJString(AObj: TObject): string;
    class function FileToObject<T: class, constructor>(const AFileName: string): T;
    class procedure ObjectToFile(AObj: TObject; const AFileName: string);
  end;

  TJSONValueHelper = class helper for TJSONValue
  strict private
  private
    function GetS(const APath: string): string;
    procedure SetS(const APath, AValue: string);
  public
    property S[const APath: string]: string read GetS write SetS;
  end;

implementation

uses
  REST.Json,
  System.DateUtils,
  System.IOUtils,
  System.SysUtils,
  System.TypInfo;


{ TJSONValueHelper }
type
  TJSONStringHack = class(TJSONString);

function TJSONValueHelper.GetS(const APath: string): string;
begin
  if (not Self.TryGetValue<string>(APath, Result)) then
    Result := string.Empty;
end;

procedure TJSONValueHelper.SetS(const APath, AValue: string);
var
  LValue: TJSONValue;
begin
  LValue := Self.FindValue(APath);
  if (LValue is TJSONString) then
  begin
    TJSONStringHack(LValue).FStrBuffer.Clear();
    TJSONStringHack(LValue).FStrBuffer.Append(AValue);
  end;
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
  // какашечка
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

function TBaseJson.AsJson: string;
begin
  if Assigned(FJSON) then
    Result := FJSON.ToJSON
  else
    Result := '';
end;

constructor TBaseJson.Create(const AJson: string);
begin
  inherited Create;
  SetJson(AJson);
end;

function TBaseJson.ReadToArray<TI>(TgClass: TBaseJsonClass; const AKey: string):
  TArray<TI>;
var
  LJsonArray: TJSONArray;
  I: Integer;
  GUID: TGUID;
begin
  // stage 1: type checking
  // cache value fot further use
  GUID := GetTypeData(TypeInfo(TI))^.GUID;
  // check for TI interface support
  if TgClass.GetInterfaceEntry(GUID) = nil then
    raise Exception.Create('GetArrayFromMethod: unsupported interface for ' +
      TgClass.ClassName);
  // stage 2: proceed data
  LJsonArray := FJSON.GetValue(AKey) as TJSONArray;
  if (not Assigned(LJsonArray)) or LJsonArray.Null then
    Exit(nil);
  SetLength(Result, LJsonArray.Count);
  for I := 0 to High(Result) do
  begin
    TgClass.GetTgClass.Create(LJsonArray.Items[I].ToString).GetInterface(GUID, Result[I]);
  end;
end;

function TBaseJson.ReadToClass<T>(const AKey: string): T;
var
  LValue: string;
  LObj: TJSONValue;
begin
  Result := nil;
  LObj := FJSON.GetValue(AKey);
  if Assigned(LObj) and (not LObj.Null) then
  begin
{$IFDEF DELPHIXE7_UP}
    LValue := LObj.ToJSON;
{$ELSE}
    LValue := LObj.ToString;
{$ENDIF}
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

class function TBaseJson.GetTgClass: TBaseJsonClass;
begin
  Result := Self;
end;

function TBaseJson.ReadToDateTime(const AKey: string): TDateTime;
var
  LValue: Int64;
begin
  Result := 0;
  if FJSON.TryGetValue<Int64>(AKey, LValue) then
    Result := UnixToDateTime(LValue, False);
end;

function TBaseJson.ReadToSimpleType<T>(const AKey: string): T;
begin
  if (not Assigned(FJSON)) or (not FJSON.TryGetValue<T>(AKey, Result)) then
    Result := Default(T);
end;

procedure TBaseJson.SetJson(const AJson: string);
begin
  FJsonRaw := AJson;
  if FJsonRaw.IsEmpty then
    Exit;
  if Assigned(FJSON) then
    FreeAndNil(FJSON);
  FJSON := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
end;

class procedure TBaseJson.UnSupported;
begin
  raise Exception.Create('Telegram method not supported in TelegaPi Library. Sorry.');
end;

procedure TBaseJson.Write(const AKey, AValue: string);
var
  JoX: TJSONPair;
begin
  JoX := FJSON.GetValue<TJSONPair>(AKey);
  if Assigned(JoX.JsonValue) then
    JoX.JsonValue.Free;
  JoX.JsonValue := TJSONString.Create(AValue);
end;

end.

