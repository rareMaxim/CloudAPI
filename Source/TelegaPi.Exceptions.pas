unit TelegaPi.Exceptions;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  TelegaPi.Types, TelegAPi.Types.Intf;

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
    FParameters: ItgResponseParameters;
    FRawData: string;
    FSendedParams: TDictionary<string, TValue>;
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="TelegaPi.Exceptions|EApiRequestException" />
    ///    class.
    /// </summary>
    /// <param name="AMessage">
    ///   The message that describes the error.
    /// </param>
    constructor Create(const AMessage: string); overload;
    constructor Create(const AMessage: string; AErrorCode: Integer); overload;
    constructor Create(const AMessage: string; AErrorCode: Integer; ASentParam: TDictionary<string, TValue>); overload;
    function ToString: string; override;
    /// <summary>
    ///   Gets the error code.
    /// </summary>
    /// <seealso href="https://telegram.wiki/bots/errorcodes">
    ///   Bot Error Codes
    /// </seealso>
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    /// <summary>
    ///   Contains information about why a request was unsuccessfull.
    /// </summary>
    property Parameters: ItgResponseParameters read FParameters write FParameters;
    property SentParams: TDictionary<string, TValue> read FSendedParams;
    property RawData: string read FRawData write FRawData;
  end;

implementation

uses
  DJSON;
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

end.

