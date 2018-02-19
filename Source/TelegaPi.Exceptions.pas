unit TelegaPi.Exceptions;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  TelegaPi.Types,
  TelegaPi.Base;

type
  ETelegramException = class(Exception);

  ETelegramTokenEmpty = class(ETelegramException);

  ETelegramDataConvert = class(ETelegramException);

  ETelegramUnknownData = class(ETelegramDataConvert);

  /// <summary>
  /// Represents an api error
  /// </summary>
  EApiRequestException = class(Exception)
  private
    FErrorCode: Integer;
    FParameters: ItgResponseParameters;
    FRawData: string;
    FSendedParams: TDictionary<string, TValue>;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TelegaPi.Exceptions|EApiRequestException" />
    /// class.
    /// </summary>
    /// <param name="AMessage">
    /// The message that describes the error.
    /// </param>
    constructor Create(const AMessage: string); overload;
    constructor Create(const AMessage: string; AErrorCode: Integer); overload;
    constructor Create(const AMessage: string; AErrorCode: Integer; ASentParam: TDictionary<string, TValue>); overload;
    function ToString: string; override;
    /// <summary>
    /// Gets the error code.
    /// </summary>
    /// <seealso href="https://telegram.wiki/bots/errorcodes">
    /// Bot Error Codes
    /// </seealso>
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    /// <summary>
    /// Contains information about why a request was unsuccessfull.
    /// </summary>
    property Parameters: ItgResponseParameters read FParameters write FParameters;
    property SentParams: TDictionary<string, TValue> read FSendedParams;
    property RawData: string read FRawData write FRawData;
  end;

  ItgExceptionHandler = interface
    ['{B5F170D3-2CFB-42FA-941A-F0781A41D053}']
    procedure HaveApiExeption(const Method: string; AException: EApiRequestException);
    procedure HaveGlobalExeption(const Method: string; AException: Exception);
  end;

  TtgExceptionManagerBase = class(TtgAbstractComponent, ItgExceptionHandler)
  public
    procedure HaveApiExeption(const Method: string; AException: EApiRequestException); virtual; abstract;
    procedure HaveGlobalExeption(const Method: string; AException: Exception); virtual; abstract;
  end;

  TtgExceptionManagerConsole = class(TtgExceptionManagerBase)
  private
    FOnApiException: TProc<string, EApiRequestException>;
    FOnGlobalException: TProc<string, Exception>;
  public
    procedure HaveApiExeption(const Method: string; AException: EApiRequestException); override;
    procedure HaveGlobalExeption(const Method: string; AException: Exception); override;
    property OnApiException: TProc<string, EApiRequestException> read FOnApiException write FOnApiException;
    property OnGlobalException: TProc<string, Exception> read FOnGlobalException write FOnGlobalException;
  end;

  TtgOnReceiveApiError = procedure(ASender: TObject; const AMethod: string; AApiRequestException: EApiRequestException) of object;

  TtgOnReceiveGlobalError = procedure(ASender: TObject; const AMethod: string; AException: Exception) of object;

  TtgExceptionManagerUI = class(TtgExceptionManagerBase)
  private
    FOnApiException: TtgOnReceiveApiError;
    FOnGlobalException: TtgOnReceiveGlobalError;
  public
    procedure HaveApiExeption(const Method: string; AException: EApiRequestException); override;
    procedure HaveGlobalExeption(const Method: string; AException: Exception); override;
  published
    property OnApiException: TtgOnReceiveApiError read FOnApiException write FOnApiException;
    property OnGlobalException: TtgOnReceiveGlobalError read FOnGlobalException write FOnGlobalException;
  end;

implementation

{ EApiRequestException }

constructor EApiRequestException.Create(const AMessage: string);
begin
  inherited Create(AMessage);
end;

constructor EApiRequestException.Create(const AMessage: string; AErrorCode: Integer);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;
end;

constructor EApiRequestException.Create(const AMessage: string; AErrorCode: Integer; ASentParam: TDictionary<string, TValue>);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;
  FSendedParams := ASentParam;
end;

function EApiRequestException.ToString: string;
const
  CFORMAT = 'EApiRequestException:%S   ErrorCode = %d. Message = %s%S%S';
  CResponseParameters = 'ResponseParameters: MigrateToChatId=%D. RetryAfter=%D';
var
  tmp: string;
begin
  if Assigned(Parameters) then
    tmp := Format(CResponseParameters, [Parameters.MigrateToChatId, Parameters.RetryAfter]);
  Result := Format(CFORMAT, [#13#10, FErrorCode, Message, #13#10, tmp])
end;

{ TtgExceptionManagerConsole }

procedure TtgExceptionManagerConsole.HaveApiExeption(const Method: string; AException: EApiRequestException);
begin
  inherited;
  if Assigned(OnApiException) then
    OnApiException(Method, AException)
  else
    raise AException;
//  AException.Free;
end;

procedure TtgExceptionManagerConsole.HaveGlobalExeption(const Method: string; AException: Exception);
begin
  inherited;
  if Assigned(OnGlobalException) then
    OnGlobalException(Method, AException)
  else
    raise AException;
 // AException.Free;
end;

{ TtgExceptionManagerUI }

procedure TtgExceptionManagerUI.HaveApiExeption(const Method: string; AException: EApiRequestException);
begin
  inherited;
  if Assigned(OnGlobalException) then
    OnApiException(Self, Method, AException)
  else
    raise AException;
//  AException.Free;
end;

procedure TtgExceptionManagerUI.HaveGlobalExeption(const Method: string; AException: Exception);
begin
  inherited;
  if Assigned(OnGlobalException) then
    OnGlobalException(Self, Method, AException)
  else
    raise AException;
//  AException.Free;
end;

end.

