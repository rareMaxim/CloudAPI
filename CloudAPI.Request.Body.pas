unit CloudAPI.Request.Body;

interface

uses
  System.Classes,
  System.Net.Mime;

type
  TcaRequestBodyType = (None, FormData, x_www_form_urlEncoded, Raw, Binary);

  TcaRequestBody = class
  private
    FType: TcaRequestBodyType;
    FRaw: TStringList;
    FFormData: TMultipartFormData;
    Fx_www_form_urlEncoded: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property &Type: TcaRequestBodyType read FType write FType default TcaRequestBodyType.None;
    property Raw: TStringList read FRaw write FRaw;
    property FormData: TMultipartFormData read FFormData write FFormData;
  end;

implementation

{ TcaRequestBody }

constructor TcaRequestBody.Create;
begin
  FType := TcaRequestBodyType.None;
  FRaw := TStringList.Create;
  FFormData := TMultipartFormData.Create();
end;

destructor TcaRequestBody.Destroy;
begin
  FRaw.Free;
  FFormData.Free;
  inherited;
end;

end.
