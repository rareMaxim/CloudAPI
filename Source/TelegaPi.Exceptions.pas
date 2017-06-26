unit TelegaPi.Exceptions;

interface

uses
  System.SysUtils,
  TelegaPi.Types;

type
  ETelegramException = class(Exception);

  ETelegramTokenEmpty = class(ETelegramException);

  ETelegramDataConvert = class(ETelegramException);

  ETelegramUnknownData = class(ETelegramDataConvert);

  /// <summary>
  ///   Represents an api error
  /// </summary>
  EApiRequestException = class(Exception)
  private
    FErrorCode: Integer;
    FParameters: TrgResponseParameters;
    FRawData: string;
  public
    class function FromApiResponse<T>(AApiResponse: TtgApiResponse<T>): EApiRequestException;
    /// <summary>
    ///   Initializes a new instance of the <see cref="ApiRequestException" />
    ///   class.
    /// </summary>
    /// <param name="AMessage">
    ///   The message that describes the error.
    /// </param>
    constructor Create(const AMessage: string); overload;
    constructor Create(const AMessage: string; AErrorCode: Integer); overload;
    function ToString: string; override;
    destructor Destroy; override;
    /// <summary>
    ///   Gets the error code.
    /// </summary>
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    /// <summary>
    ///   Contains information about why a request was unsuccessfull.
    /// </summary>
    property Parameters: TrgResponseParameters read FParameters write FParameters;
    property RawData: string read FRawData write FRawData;
  end;

implementation

uses
  XSuperObject;
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

destructor EApiRequestException.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

class function EApiRequestException.FromApiResponse<T>(AApiResponse: TtgApiResponse<T>): EApiRequestException;
begin
  Result := EApiRequestException.Create(AApiResponse.Message, AApiResponse.Code);
  Result.Parameters := AApiResponse.Parameters;
  Result.RawData := AApiResponse.AsJSON();
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

end.

