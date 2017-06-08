unit TelegaPi.Exceptions;

interface

uses
  System.SysUtils,
  TelegaPi.Types;

Type
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
    FParameters: TrgResponseParameters;
  public
    class function FromApiResponse<T>(AApiResponse: TtgApiResponse<T>)
      : EApiRequestException;
    /// <summary>
    /// Initializes a new instance of the <see cref="ApiRequestException"/> class.
    /// </summary>
    /// <param name="AMessage">The message that describes the error.</param>
    constructor Create(AMessage: String); overload;
    constructor Create(AMessage: String; AErrorCode: Integer); overload;
    /// <summary>
    /// Gets the error code.
    /// </summary>
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    /// <summary>
    /// Contains information about why a request was unsuccessfull.
    /// </summary>
    property Parameters: TrgResponseParameters read FParameters
      write FParameters;

  end;

implementation

{ EApiRequestException }

constructor EApiRequestException.Create(AMessage: String);
begin
  inherited Create(AMessage);
end;

constructor EApiRequestException.Create(AMessage: String; AErrorCode: Integer);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;

end;

class function EApiRequestException.FromApiResponse<T>
  (AApiResponse: TtgApiResponse<T>): EApiRequestException;
begin
  Result := EApiRequestException.Create(AApiResponse.Message,
    AApiResponse.Code);
  Result.Parameters := AApiResponse.Parameters;
end;

end.
