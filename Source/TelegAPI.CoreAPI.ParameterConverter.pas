unit TelegAPI.CoreAPI.ParameterConverter;

interface

uses
  System.Generics.Collections,
  System.Net.Mime,
  System.rtti,
  System.SysUtils,
  System.TypInfo,
  TelegAPI.Types,
  TelegAPI.CoreAPI.Parameter;

type
  /// <summary>
  ///   This class is used by func. TTelegramBotCore.ParamsToFormData to locate
  ///   suitable param loader for API request parameters preparation.
  /// </summary>
  TtgParamConverter = class
  public
    type
      //parameter loader method
      TLoader = procedure(var AFormData: TMultipartFormData; AParam: TtgApiParameter) of object;
  protected
    procedure AddInteger(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
    procedure AddString(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
    procedure AddInt64(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
    procedure AddBoolean(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
    procedure AddClass_TtgFileToSend(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
  public
    ParamLoaders: TDictionary<PTypeInfo, TtgParamConverter.TLoader>;
    constructor Create;
    destructor Destroy; override;
    function IsSupported(Param: TtgApiParameter): Boolean;
    function ApplyParamToFormData(const AParam: TtgApiParameter; var Form: TMultipartFormData): Boolean;
  end;

implementation

uses
  TelegAPI.Helpers;

procedure TtgParamConverter.AddInteger(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsInteger.ToString);
end;

procedure TtgParamConverter.AddString(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsString);
end;

function TtgParamConverter.ApplyParamToFormData(const AParam: TtgApiParameter; var Form: TMultipartFormData): Boolean;
begin
  Result := ParamLoaders.ContainsKey(AParam.Value.TypeInfo);
  if not Result then
    Exit;
  ParamLoaders[AParam.Value.TypeInfo](Form, AParam);
end;

constructor TtgParamConverter.Create;
begin
  //init type-lookup dictionary
  ParamLoaders := TDictionary<PTypeInfo, TtgParamConverter.TLoader>.Create;
  //primitive types
  ParamLoaders.Add(PTypeInfo(TypeInfo(Integer)), AddInteger);
  ParamLoaders.Add(PTypeInfo(TypeInfo(string)), AddString);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Int64)), AddInt64);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Boolean)), AddBoolean);
  //class types
  ParamLoaders.Add(PTypeInfo(TypeInfo(TtgFileToSend)), AddClass_TtgFileToSend);
end;

destructor TtgParamConverter.Destroy;
begin
  ParamLoaders.Free;
  inherited;
end;

function TtgParamConverter.IsSupported(Param: TtgApiParameter): Boolean;
begin
  Result := ParamLoaders.ContainsKey(Param.Value.TypeInfo);
end;

procedure TtgParamConverter.AddInt64(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsInt64.ToString);
end;

procedure TtgParamConverter.AddBoolean(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsBoolean.ToString(TUseBoolStrs.True));
end;

procedure TtgParamConverter.AddClass_TtgFileToSend(var AFormData: TMultipartFormData; AParam: TtgApiParameter);
var
  LFileToSent: TtgFileToSend;
begin
  LFileToSent := AParam.Value.AsType<TtgFileToSend>;
  try
    if LFileToSent.Tag = TtgFileToSend.FILE_TO_SEND_STREAM then
      AFormData.AddStream(AParam.Key, LFileToSent.Content, LFileToSent.Data)
    else if LFileToSent.Tag = TtgFileToSend.FILE_TO_SEND_FILE then
      AFormData.AddFile(AParam.Key, LFileToSent.Data)
    else if (LFileToSent.Tag = TtgFileToSend.FILE_TO_SEND_ID) or (LFileToSent.Tag = TtgFileToSend.FILE_TO_SEND_URL) then
      AFormData.AddField(AParam.Key, LFileToSent.Data)
    else
      raise Exception.Create('Cant convert TTgFileToSend: Unknown prototype tag');
  finally
    LFileToSent.Free;
  end;
end;

end.

