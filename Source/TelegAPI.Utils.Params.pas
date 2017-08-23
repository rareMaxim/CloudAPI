unit TelegAPI.Utils.Params;

interface

uses
  System.rtti,
  System.TypInfo,
  System.SysUtils,
  TelegAPI.Types,
  TelegAPI.Exceptions,
  System.Net.Mime,
  System.Generics.Collections;

type
  /// <summary>
  ///  This class is used by func. TTelegramBotCore.ParamsToFormData
  ///  to locate suitable param loader for API request parameters preparation.
  /// </summary>
  TtgParamLoader = class
  public
    type
      //parameter loader method
      TLoader = procedure(var AFormData: TMultipartFormData; TypeInfo: PTypeInfo; const AKey: string; AValue: TValue) of object;
  public
    ParamLoaders: TDictionary<PTypeInfo, TtgParamLoader.TLoader>;
  protected
    procedure AddInteger(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
    procedure AddString(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
    procedure AddInt64(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
    procedure AddBoolean(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
    procedure AddClass_TtgFileToSend(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  TelegAPI.Helpers;

procedure TtgParamLoader.AddInteger(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
begin
  if AValue.AsInteger <> 0 then
    AFormData.AddField(AKey, AValue.AsInteger.ToString);
end;

procedure TtgParamLoader.AddString(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
begin
  if not AValue.AsString.IsEmpty then
    AFormData.AddField(AKey, AValue.AsString);
end;

constructor TtgParamLoader.Create;
begin
  //init type-lookup dictionary
  ParamLoaders := TDictionary<PTypeInfo, TtgParamLoader.TLoader>.Create;
  //primitive types
  ParamLoaders.Add(PTypeInfo(TypeInfo(Integer)), AddInteger);
  ParamLoaders.Add(PTypeInfo(TypeInfo(string)), AddString);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Int64)), AddInt64);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Boolean)), AddBoolean);
  //class types
  ParamLoaders.Add(PTypeInfo(TypeInfo(TtgFileToSend)), AddClass_TtgFileToSend);
end;

destructor TtgParamLoader.Destroy;
begin
  ParamLoaders.Free;
  inherited;
end;

procedure TtgParamLoader.AddInt64(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
begin
  if AValue.AsInt64 <> 0 then
    AFormData.AddField(AKey, AValue.AsInt64.ToString);
end;

procedure TtgParamLoader.AddBoolean(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
begin
  if AValue.AsBoolean then
    AFormData.AddField(AKey, AValue.AsBoolean.ToString(TUseBoolStrs.True));
end;

procedure TtgParamLoader.AddClass_TtgFileToSend(var AFormData: TMultipartFormData; ATypeInfo: PTypeInfo; const AKey: string; AValue: TValue);
var
  LFileToSent: TtgFileToSend;
begin
  LFileToSent := AValue.AsType<TtgFileToSend>;
  if Assigned(LFileToSent.Content) then
    AFormData.AddStream(AKey, LFileToSent.Content)
  else
    AFormData.AddFile(AKey, LFileToSent.FileName);
end;

end.

