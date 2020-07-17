unit CloudAPI.Exceptions;

interface

uses
  CloudAPI.Parameter,
  System.SysUtils;

type
  ECloudApiException = class(Exception)
  private
    FCodeInt: Integer;
    FCodeStr: string;
    FMessage: string;
  public
    constructor Create(const ACode, AMessage: string); overload;
    constructor Create(const ACode: Integer; const AMessage: string); overload;
  published
    property CodeInt: Integer read FCodeInt write FCodeInt;
    property CodeStr: string read FCodeStr write FCodeStr;
    property Message: string read FMessage write FMessage;
  end;

  ECloudApiRequairedParameterException = class(ECloudApiException)
  private const
    C_MESSAGE = 'Parameter "%s" is requaired!';
  public
    constructor Create(AParameter: TcaParameter);
  end;

implementation

{ ECloudApiRequairedParameterException }

constructor ECloudApiRequairedParameterException.Create(AParameter: TcaParameter);
begin
  inherited Create('CloudAPI', Format(C_MESSAGE, [AParameter.Name]));
end;

{ ECloudApiException }

constructor ECloudApiException.Create(const ACode, AMessage: string);
begin
  FCodeStr := ACode;
  TryStrToInt(ACode, FCodeInt);
  FMessage := AMessage;
  inherited CreateFmt('[%s] %s', [FCodeStr, FMessage]);
end;

constructor ECloudApiException.Create(const ACode: Integer; const AMessage: string);
begin
  Self.Create(ACode.ToString, AMessage);
end;

end.
