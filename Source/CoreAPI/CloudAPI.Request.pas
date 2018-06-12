unit CloudAPI.Request;

interface

uses
  System.TypInfo,
  System.Net.Mime,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes;

type
{$SCOPEDENUMS ON}
  TStoreFormat = (InFormData, InStringList, InUrl, InHeader, Auto);

  IApiRequest = interface
    ['{AF1F11B6-28DB-49EF-AD29-04634D35B15F}']
  //private
    function GetDomain: string;
    procedure SetDomain(const AValue: string);
    function GetMethodUrl: string;
    procedure SetMethodUrl(const AValue: string);
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const AValue: TProc<Exception>);
    function GetHttpClient: THTTPClient;
    procedure SetHttpClient(const AHttpClient: THTTPClient);
    function GetStoreMultipartForm: TMultipartFormData;
    procedure SetStoreMultipartForm(const AValue: TMultipartFormData);
    function GetStoreStringList: TStringList;
    procedure SetStoreStringList(const Value: TStringList);
    function GetStoreUrl: TStringList;
    procedure SetStoreUrl(const Value: TStringList);
    function GetStoreHeaders: TList<TNetHeader>;
    procedure SetStoreHeaders(const Value: TList<TNetHeader>);
    function GetOnDataSend: TProc<string, string, string>;
    procedure SetOnDataSend(const Value: TProc<string, string, string>);
    function GetOnDataReceiveAsString: TFunc<string, string>;
    procedure SetOnDataReceiveAsString(const Value: TFunc<string, string>);
    function GetStoreAutoFormat: TStoreFormat;
    procedure SetStoreAutoFormat(const Value: TStoreFormat);
  //public
    {$REGION 'Add Parameter'}
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      string; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Int64;
      const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      TObject; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      Single; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      Boolean; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    {$ENDREGION}
    function ClearParams: IApiRequest;
    function Execute: IHTTPResponse;
    function ExecuteAsString: string;
    function ExecuteAsBytes: TArray<Byte>;
    function ExecuteAndReadValue: string;
    function ExecuteAsBool: Boolean;
    function SetMethod(const AValue: string): IApiRequest;
    property Domain: string read GetDomain write SetDomain;
    property MethodUrl: string read GetMethodUrl write SetMethodUrl;
    property HttpClient: THTTPClient read GetHttpClient write SetHttpClient;
    property StoreAutoFormat: TStoreFormat read GetStoreAutoFormat write SetStoreAutoFormat;
    property StoreMultipartForm: TMultipartFormData read GetStoreMultipartForm write SetStoreMultipartForm;
    property StoreStringList: TStringList read GetStoreStringList write SetStoreStringList;
    property StoreUrl: TStringList read GetStoreUrl write SetStoreUrl;
    property StoreHeaders: TList<TNetHeader> read GetStoreHeaders write SetStoreHeaders;
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    property OnDataSend: TProc<string, string, string> read GetOnDataSend write SetOnDataSend;
    property OnDataReceiveAsString: TFunc<string, string> read GetOnDataReceiveAsString write SetOnDataReceiveAsString;
  end;

  TApiRequest = class(TInterfacedObject, IApiRequest)
 //   var     Converter: TDictionary<PTypeInfo, TProc<IApiRequest>>;
  strict private
    FMethod: string;
    FDomain: string;
    FStoreInFormData: TMultipartFormData;
    FStoreInStringList: TStringList;
    FStoreInUrl: TStringList;
    FStoreInHeader: TList<TNetHeader>;
    FHttpClient: THTTPClient;
    FOnError: TProc<Exception>;
    FOnDataSend: TProc<string, string, string>;
    FOnDataReceiveAsString: TFunc<string, string>;
    FStoreAutoFormat: TStoreFormat;
  private
    function GetDomain: string;
    procedure SetDomain(const AValue: string);
    function GetMethodUrl: string;
    procedure SetMethodUrl(const AValue: string);
    function GetStoreMultipartForm: TMultipartFormData;
    procedure SetStoreMultipartForm(const AValue: TMultipartFormData);
    function GetStoreStringList: TStringList;
    procedure SetStoreStringList(const Value: TStringList);
    function GetStoreUrl: TStringList;
    procedure SetStoreUrl(const Value: TStringList);
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const AValue: TProc<Exception>);
    function GetHttpClient: THTTPClient;
    procedure SetHttpClient(const AHttpClient: THTTPClient);
    function GetStoreHeaders: TList<TNetHeader>;
    procedure SetStoreHeaders(const Value: TList<TNetHeader>);
    function GetOnDataSend: TProc<string, string, string>;
    procedure SetOnDataSend(const Value: TProc<string, string, string>);
    function GetOnDataReceiveAsString: TFunc<string, string>;
    procedure SetOnDataReceiveAsString(const Value: TFunc<string, string>);
    function GetStoreAutoFormat: TStoreFormat;
    procedure SetStoreAutoFormat(const Value: TStoreFormat);
  protected
  {$IFDEF TESTINSIGHT}
  public
  {$ENDIF}
    function HeadersToString(AHeaders: TArray<TNetHeader>): string;
    function FormDataToString(AFormData: TMultipartFormData): string;
    procedure DoHaveException(E: Exception);
    function NeedAdd(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
    function RaiseArgument(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
    procedure DoStoreParam(const AKey: string; const AValue, ADefaultValue:
      string; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto);
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      Single; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      Boolean; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
  public
    {$REGION 'Add Parameter'}
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      string; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue: Int64;
      const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    function AddParameter(const AKey: string; const AValue, ADefaultValue:
      TObject; const ARequired: Boolean; const AStoreFormat: TStoreFormat = TStoreFormat.Auto): IApiRequest; overload;
    {$ENDREGION}
    function ClearParams: IApiRequest;
    function Execute: IHTTPResponse;
    function ExecuteAsString: string;
    function ExecuteAsBytes: TArray<Byte>;
    function ExecuteAndReadValue: string;
    function ExecuteAsBool: Boolean;
    function SetMethod(const AValue: string): IApiRequest;
    constructor Create;
    destructor Destroy; override;
    property Domain: string read GetDomain write SetDomain;
    property MethodUrl: string read GetMethodUrl write SetMethodUrl;
    property StoreAutoFormat: TStoreFormat read GetStoreAutoFormat write SetStoreAutoFormat;
    property StoreMultipartForm: TMultipartFormData read GetStoreMultipartForm write SetStoreMultipartForm;
    property StoreStringList: TStringList read GetStoreStringList write SetStoreStringList;
    property StoreUrl: TStringList read GetStoreUrl write SetStoreUrl;
    property StoreHeaders: TList<TNetHeader> read GetStoreHeaders write SetStoreHeaders;
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    {Url, Body, Header}
    property OnDataSend: TProc<string, string, string> read GetOnDataSend write SetOnDataSend;
    property OnDataReceiveAsString: TFunc<string, string> read GetOnDataReceiveAsString write SetOnDataReceiveAsString;
  end;

  TMultipartFormDataHelper = class helper for TMultipartFormData
    /// <summary>
    ///   Add a form data Stream
    /// </summary>
    /// <param name="AFieldName">
    ///   Field Name
    /// </param>
    /// <param name="Data">
    ///   Stream
    /// </param>
    /// <param name="AFileName">
    ///   file name: "File.ext"
    /// </param>
    procedure AddStream(const AFieldName: string; Data: TStream; const AFileName: string);
  end;

implementation

uses
  System.IOUtils,
  CloudAPI.Utils.Json;

const
  ERR_TWICE_POST_STORE = 'Попытка использовать разные хранилища для Post запроса';
  ERR_CANT_SETUP_STORE_AUTO = 'Нельзя использовать это значение, попробуйте другое';
{ TApiRequest }

function TApiRequest.RaiseArgument(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
begin
  Result := ARequired and AValue.Equals(ADefaultValue);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue,
  ADefaultValue: Int64; const ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, AValue.ToString, ADefaultValue.ToString, ARequired, AStoreFormat);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue,
  ADefaultValue: TObject; const ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, TJsonUtils.ObjectToJString(AValue),  //
    TJsonUtils.ObjectToJString(ADefaultValue), ARequired, AStoreFormat);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue,
  ADefaultValue, ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, AValue.ToString, ADefaultValue.ToString, ARequired, AStoreFormat);
end;

function TApiRequest.AddParameter(const AKey: string; const AValue,
  ADefaultValue: Single; const ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  Result := AddParameter(AKey, AValue.ToString, ADefaultValue.ToString, ARequired, AStoreFormat);
end;

function TApiRequest.ClearParams: IApiRequest;
begin
  FStoreInFormData.Free;
  FStoreInFormData := TMultipartFormData.Create;
  FStoreInStringList.Clear;
  FStoreInUrl.Clear;
  FStoreInHeader.Clear;
  Result := Self;
end;

constructor TApiRequest.Create;
begin
  FStoreInFormData := TMultipartFormData.Create;
  FStoreInStringList := TStringList.Create;
  FStoreInUrl := TStringList.Create;
  FStoreInHeader := TList<TNetHeader>.Create;
  FHttpClient := THTTPClient.Create;
  FHttpClient.AllowCookies := True;
  FStoreAutoFormat := TStoreFormat.InFormData;
end;

destructor TApiRequest.Destroy;
begin
  FreeAndNil(FStoreInFormData);
  FreeAndNil(FStoreInStringList);
  FreeAndNil(FStoreInUrl);
  FreeAndNil(FStoreInHeader);
  FreeAndNil(FHttpClient);
  inherited;
end;

procedure TApiRequest.DoHaveException(E: Exception);
begin
  if Assigned(OnError) then
    OnError(E)
  else
    raise E;
end;

procedure TApiRequest.DoStoreParam(const AKey, AValue, ADefaultValue: string;
  const ARequired: Boolean; const AStoreFormat: TStoreFormat);
begin
  if RaiseArgument(AValue, ADefaultValue, ARequired) then
    raise EArgumentException.Create('AValue = ADefaultValue');
  if not NeedAdd(AValue, ADefaultValue, ARequired) then
    Exit;
  case AStoreFormat of
    TStoreFormat.InFormData:
      FStoreInFormData.AddField(AKey, AValue);
    TStoreFormat.InStringList:
      FStoreInStringList.AddPair(AKey, AValue);
    TStoreFormat.InUrl:
      FStoreInUrl.AddPair(AKey, AValue);
    TStoreFormat.InHeader:
      FStoreInHeader.Add(TNetHeader.Create(AKey, AValue));
  else
    raise ENotImplemented.Create('Unknown StoreFormat');
  end;
end;

function TApiRequest.Execute: IHTTPResponse;
var
  LFullUrl: string;
  LPostRunned: Boolean; //Отправлено из TMultipartFormData
begin
  Result := nil;
  LPostRunned := False;
  try
    LFullUrl := string.Join('/', [Domain, MethodUrl]) + '?' + string.Join('&', FStoreInUrl.ToStringArray);
    if FStoreInFormData.Stream.Size > 44{wtf} then
    begin
      if Assigned(OnDataSend) then
        OnDataSend(LFullUrl, '', HeadersToString(FStoreInHeader.ToArray));
      Result := FHttpClient.Post(LFullUrl, FStoreInFormData, nil, FStoreInHeader.ToArray);
      LPostRunned := True;
    end;
    if FStoreInStringList.Count > 0 then
    begin
      if LPostRunned then
        DoHaveException(Exception.Create(ERR_TWICE_POST_STORE))
      else
      begin
        if Assigned(OnDataSend) then
          OnDataSend(LFullUrl, FormDataToString(FStoreInFormData), HeadersToString(FStoreInHeader.ToArray));
        Result := FHttpClient.Post(LFullUrl, FStoreInStringList, nil, nil, FStoreInHeader.ToArray);
        LPostRunned := True;
      end;
    end;
    if not LPostRunned then
    begin
      if Assigned(OnDataSend) then
        OnDataSend(LFullUrl, '', HeadersToString(FStoreInHeader.ToArray));
      Result := FHttpClient.Get(LFullUrl, nil, FStoreInHeader.ToArray);
    end;
    ClearParams;
  except
    on E: Exception do
      DoHaveException(E);
  end;
end;

function TApiRequest.ExecuteAndReadValue: string;
var
  LJson: TBaseJson;
begin
  LJson := TBaseJson.Create(ExecuteAsString);
  try
    Result := LJson.asString;
  finally
    LJson.Free;
  end;
end;

function TApiRequest.ExecuteAsBool: Boolean;
var
  LJson: TBaseJson;
begin
  LJson := TBaseJson.Create(ExecuteAsString);
  try
    Result := LJson.AsBoolean;
  finally
    LJson.Free;
  end;
end;

function TApiRequest.ExecuteAsBytes: TArray<Byte>;
var
  LResponse: IHTTPResponse;
begin
  LResponse := Execute;
  LResponse.ContentStream.Position := 0;
  SetLength(Result, LResponse.ContentStream.Size);
  LResponse.ContentStream.Read(Result[0], LResponse.ContentStream.Size);
end;

function TApiRequest.ExecuteAsString: string;
begin
  Result := Execute.ContentAsString();
  if Assigned(OnDataReceiveAsString) then
    Result := OnDataReceiveAsString(Result);
end;

function TApiRequest.FormDataToString(AFormData: TMultipartFormData): string;
var
  LStrList: TStringList;
begin
  LStrList := TStringList.Create;
  try
  //  AFormData.Stream.Position := 0;
    LStrList.LoadFromStream(AFormData.Stream);
    Result := LStrList.Text;
  finally
    LStrList.Free;
  end;
end;

function TApiRequest.NeedAdd(const AValue, ADefaultValue: string; const ARequired: Boolean): Boolean;
begin
  Result := ARequired or (not AValue.Equals(ADefaultValue));
end;

function TApiRequest.AddParameter(const AKey, AValue, ADefaultValue: string;
  const ARequired: Boolean; const AStoreFormat: TStoreFormat): IApiRequest;
begin
  DoStoreParam(AKey, AValue, ADefaultValue, ARequired, AStoreFormat);
  Result := Self;
end;

function TApiRequest.GetHttpClient: THTTPClient;
begin
  Result := FHttpClient;
end;

function TApiRequest.GetMethodUrl: string;
begin
  Result := FMethod;
end;

function TApiRequest.GetOnDataReceiveAsString: TFunc<string, string>;
begin
  Result := FOnDataReceiveAsString;
end;

function TApiRequest.GetOnDataSend: TProc<string, string, string>;
begin
  Result := FOnDataSend;
end;

function TApiRequest.GetOnError: TProc<Exception>;
begin
  Result := FOnError;
end;

function TApiRequest.GetStoreAutoFormat: TStoreFormat;
begin
  Result := FStoreAutoFormat;
end;

function TApiRequest.GetStoreHeaders: TList<TNetHeader>;
begin
  Result := FStoreInHeader;
end;

function TApiRequest.GetStoreMultipartForm: TMultipartFormData;
begin
  Result := FStoreInFormData;
end;

function TApiRequest.GetStoreStringList: TStringList;
begin
  Result := FStoreInStringList;
end;

function TApiRequest.GetStoreUrl: TStringList;
begin
  Result := FStoreInUrl;
end;

function TApiRequest.HeadersToString(AHeaders: TArray<TNetHeader>): string;
var
  LHeader: TNameValuePair;
begin
  Result := '';
  for LHeader in AHeaders do
  begin
    Result := Result + LHeader.Name + '=' + LHeader.Value + #13#10;
  end;
end;

function TApiRequest.GetDomain: string;
begin
  Result := FDomain;
end;

procedure TApiRequest.SetHttpClient(const AHttpClient: THTTPClient);
begin
  FHttpClient := AHttpClient;
end;

function TApiRequest.SetMethod(const AValue: string): IApiRequest;
begin
  MethodUrl := AValue;
  Result := Self;
end;

procedure TApiRequest.SetMethodUrl(const AValue: string);
begin
  FMethod := AValue;
end;

procedure TApiRequest.SetOnDataReceiveAsString(const Value: TFunc<string, string>);
begin
  FOnDataReceiveAsString := Value;
end;

procedure TApiRequest.SetOnDataSend(const Value: TProc<string, string, string>);
begin
  FOnDataSend := Value;
end;

procedure TApiRequest.SetOnError(const AValue: TProc<Exception>);
begin
  FOnError := AValue;
end;

procedure TApiRequest.SetStoreAutoFormat(const Value: TStoreFormat);
begin
  if Value = TStoreFormat.Auto then
    DoHaveException(Exception.Create(ERR_CANT_SETUP_STORE_AUTO));
  FStoreAutoFormat := Value;
end;

procedure TApiRequest.SetStoreHeaders(const Value: TList<TNetHeader>);
begin
  FStoreInHeader := Value;
end;

procedure TApiRequest.SetStoreMultipartForm(const AValue: TMultipartFormData);
begin
  FStoreInFormData := AValue;
end;

procedure TApiRequest.SetStoreStringList(const Value: TStringList);
begin
  FStoreInStringList := Value;
end;

procedure TApiRequest.SetStoreUrl(const Value: TStringList);
begin
  FStoreInUrl := Value;
end;

procedure TApiRequest.SetDomain(const AValue: string);
begin
  FDomain := AValue;
end;

{ TtgTMultipartFormDataHelper }

procedure TMultipartFormDataHelper.AddStream(const AFieldName: string; Data: TStream; const AFileName: string);
var
  LFileStream: TFileStream;
  LTmpDir: string;
  LTmpFilename: string;
begin
  //get filename for tmp folder e.g. ..\AppData\local\temp\4F353A8AC6AB446D9F592A30B157291B
  LTmpDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + TPath.GetGUIDFileName(False);
  LTmpFilename := IncludeTrailingPathDelimiter(LTmpDir) + ExtractFileName(AFileName);
  try
    TDirectory.CreateDirectory(LTmpDir);
    try
      LFileStream := TFileStream.Create(LTmpFilename, fmCreate);
      try
        LFileStream.CopyFrom(Data, 0);
      finally
        LFileStream.Free;
      end;
      AddFile(AFieldName, LTmpFilename);
    finally
      TFile.Delete(LTmpFilename);
    end;
  finally
    TDirectory.Delete(LTmpDir);
  end;
end;

end.

