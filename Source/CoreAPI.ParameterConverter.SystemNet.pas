unit CoreAPI.ParameterConverter.SystemNet;

interface

{/$IFNDEF USE_INDY}

uses
  System.Generics.Collections,
  System.Net.Mime,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  TelegAPI.Types,
  CoreAPI.Parameter;

type
  /// <summary>
  /// This class is used by func. TTelegramBotCore.ParamsToFormData to locate
  /// suitable param loader for API request parameters preparation.
  /// </summary>
  TtgParamConverter = class
  public type
    // parameter loader method
    TLoader = procedure(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter) of object;
  protected
    procedure AddInteger(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter);
    procedure AddTDateTime(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter);
    procedure AddString(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter);
    procedure AddInt64(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter);
    procedure AddBoolean(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter);
    procedure AddClass_TtgFileToSend(var AFormData: TMultipartFormData;
      AParam: TtgApiParameter);
  public
    ParamLoaders: TDictionary<PTypeInfo, TtgParamConverter.TLoader>;
    constructor Create;
    destructor Destroy; override;
    function IsSupported(Param: TtgApiParameter): Boolean;
    function ApplyParamToFormData(const AParam: TtgApiParameter;
      var Form: TMultipartFormData): Boolean;
  end;
{/$ENDIF}

implementation

{/$IFNDEF USE_INDY}

uses
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  TelegAPI.Helpers;

type
  TtgTMultipartFormDataHelper = class helper for TMultipartFormData
    /// <summary>
    /// Add a form data Stream
    /// </summary>
    /// <param name="AFieldName">
    /// Field Name
    /// </param>
    /// <param name="Data">
    /// Stream
    /// </param>
    /// <param name="AFileName">
    /// file name: "File.ext"
    /// </param>
    procedure AddStream(const AFieldName: string; Data: TStream;
      const AFileName: string = '');
  end;

  { TtgTMultipartFormDataHelper }

procedure TtgTMultipartFormDataHelper.AddStream(const AFieldName: string;
  Data: TStream; const AFileName: string);
var
  LFileStream: TFileStream;
  LTmpDir: string;
  LTmpFilename: string;
begin
  // get filename for tmp folder e.g. ..\AppData\local\temp\4F353A8AC6AB446D9F592A30B157291B
  LTmpDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) +
    TPath.GetGUIDFileName(false);
  LTmpFilename := IncludeTrailingPathDelimiter(LTmpDir) +
    ExtractFileName(AFileName);
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

procedure TtgParamConverter.AddInteger(var AFormData: TMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsInteger.ToString);
end;

procedure TtgParamConverter.AddString(var AFormData: TMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsString);
end;

procedure TtgParamConverter.AddTDateTime(var AFormData: TMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, DateTimeToUnix(AParam.Value.AsType<TDateTime>,
    false).ToString);
end;

function TtgParamConverter.ApplyParamToFormData(const AParam: TtgApiParameter;
  var Form: TMultipartFormData): Boolean;
begin
  Result := ParamLoaders.ContainsKey(AParam.Value.TypeInfo);
  if not Result then
    Exit;
  ParamLoaders[AParam.Value.TypeInfo](Form, AParam);
end;

constructor TtgParamConverter.Create;
begin
  // init type-lookup dictionary
  ParamLoaders := TDictionary<PTypeInfo, TtgParamConverter.TLoader>.Create;
  // primitive types
  ParamLoaders.Add(PTypeInfo(TypeInfo(Integer)), AddInteger);
  ParamLoaders.Add(PTypeInfo(TypeInfo(string)), AddString);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Int64)), AddInt64);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Boolean)), AddBoolean);
  ParamLoaders.Add(PTypeInfo(TypeInfo(TDateTime)), AddTDateTime);
  // class types
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

procedure TtgParamConverter.AddInt64(var AFormData: TMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsInt64.ToString);
end;

procedure TtgParamConverter.AddBoolean(var AFormData: TMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsBoolean.ToString
    (TUseBoolStrs.True));
end;

procedure TtgParamConverter.AddClass_TtgFileToSend(var AFormData
  : TMultipartFormData; AParam: TtgApiParameter);
var
  LFileToSent: TtgFileToSend;
begin
  LFileToSent := AParam.Value.AsType<TtgFileToSend>;
  try
    case LFileToSent.Tag of
      TtgFileToSend.FILE_TO_SEND_STREAM:
        AFormData.AddStream(AParam.Key, LFileToSent.Content, LFileToSent.Data);
      TtgFileToSend.FILE_TO_SEND_FILE:
        AFormData.AddFile(AParam.Key, LFileToSent.Data);
      TtgFileToSend.FILE_TO_SEND_ID, TtgFileToSend.FILE_TO_SEND_URL:
        AFormData.AddField(AParam.Key, LFileToSent.Data);
    else
      raise Exception.Create
        ('Cant convert TTgFileToSend: Unknown prototype tag');
    end;
  finally
    LFileToSent.Free;
  end;
end;
{/$ENDIF}

end.
