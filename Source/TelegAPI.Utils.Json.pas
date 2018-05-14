unit TelegAPI.Utils.Json;

interface

uses
  System.Json;

type
  TBaseJsonClass = class of TBaseJson;

  TBaseJson = class(TInterfacedObject)
  private
  protected
    FJSON: TJSONObject;
    function ReadToClass<T: class, constructor>(const AKey: string): T;
    function ReadToSimpleType<T>(const AKey: string): T;
    function ReadToDateTime(const AKey: string): TDateTime;
    function ReadToArray<TI: IInterface>(TgClass: TBaseJsonClass; const AKey:
      string): TArray<TI>;
  public
    class function FromJson(const AJson: string): TBaseJson;
    class function GetTgClass: TBaseJsonClass; virtual;// abstract;
    class procedure UnSupported;
    constructor Create(const AJson: string); virtual;
    destructor Destroy; override;
  end;

  TJsonUtils = class
    class function ArrayToJString<T: class>(LArray: TArray<T>): string;
    class function ObjectToJString(AObj: TObject): string;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils,
  System.DateUtils,
  REST.Json;
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
  Result := Result.Replace('"inline_keyboard":null', '', [rfReplaceAll]); // какашечка
end;

class function TJsonUtils.ObjectToJString(AObj: TObject): string;
begin  // IF DELPHI_VERSION < TOKIO
  if Assigned(AObj) then
    Result := TJson.ObjectToJsonString(AObj)
  else
    Result := 'null';
end;

{ TBaseJson }

constructor TBaseJson.Create(const AJson: string);
begin
  inherited Create;
  if AJson.IsEmpty then
    Exit;
  FJSON := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
end;

function TBaseJson.ReadToArray<TI>(TgClass: TBaseJsonClass; const AKey: string):
  TArray<TI>;
var
  LJsonArray: TJSONArray;
  I: Integer;
  GUID: TGUID;
begin
    // stage 1: type checking
    //cache value fot further use
  GUID := GetTypeData(TypeInfo(TI))^.GUID;
    //check for TI interface support
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
{$IFDEF USE_INDY}
    // Директива не совсем подходит. Это в случае если используется старая версия ИДЕ
    LValue := LObj.ToString;
{$ELSE}
    LValue := LObj.ToJSON;
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

class procedure TBaseJson.UnSupported;
begin
  raise Exception.Create('Telegram method not supported in TelegaPi Library. Sorry.');
end;

end.

