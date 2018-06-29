unit CloudAPI.Exception;

interface

uses
  System.SysUtils,
  CloudAPI.Request;

type
  ECloudApiException = class(Exception)
  private
    FCode: string;
    FDescription: string;
    FRequest: IApiRequest;
    FResponse: string;
  public
    constructor Create(const ACode, ADescription: string; ARequest: IApiRequest = nil); reintroduce; overload;
    constructor Create(const ACode: Integer; const ADescription: string;
      ARequest: IApiRequest = nil); reintroduce; overload;
    property Code: string read FCode write FCode;
    property Description: string read FDescription write FDescription;
    property Request: IApiRequest read FRequest write FRequest;
    property Response: string read FResponse write FResponse;
  end;

implementation

{ ECloudApiException }

constructor ECloudApiException.Create(const ACode, ADescription: string; ARequest: IApiRequest);
begin
  FCode := ACode;
  FDescription := ADescription;
  FRequest := ARequest;
  inherited Create(FCode + ': ' + Description);
end;

constructor ECloudApiException.Create(const ACode: Integer; const ADescription: string; ARequest: IApiRequest);
begin
  Create(ACode.ToString, ADescription, ARequest);
end;

end.

