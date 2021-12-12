unit CloudAPI.Request.New;

interface

uses
  CloudAPI.Parameter,
  CloudAPI.Request.Body,
  System.Generics.Collections,
  System.Classes;

type

  TcaRequest = class
  private
    FQueryParams: TList<TcaParameter>;
    FBody: TcaRequestBody;
    FHttpMethod: string;
  public
    constructor Create;
    destructor Destroy; override;
    property HttpMethod: string read FHttpMethod write FHttpMethod;
  end;

implementation

{ TcaRequest }

constructor TcaRequest.Create;
begin
  FQueryParams := TList<TcaParameter>.Create;
  FBody := TcaRequestBody.Create;
end;

destructor TcaRequest.Destroy;
begin
  FBody.Free;
  FQueryParams.Free;
  inherited;
end;

end.
