unit TelegAPI.Utils.Params;
interface

uses System.rtti,
     System.TypInfo,
     System.SysUtils,
     TelegAPI.Types,
     TelegAPI.Exceptions,
     System.Net.Mime;

type //parameter loader method
     TtgParamLoaderMethod = procedure(var mpfd:TMultipartFormData;
                                      TypeInfo:PTypeInfo;
                                      key:string;
                                      value:TValue) of object;

     //This class is used by func. TTelegramBotCore.ParamsToFormData
     //to locate suitable param loader for API request parameters preparation.
     TtgParamLoader=class
     public
       class procedure AddInteger (var mpfd:TMultipartFormData; Type_Info:PTypeInfo;  key:string;  value:TValue);
       class procedure AddString  (var mpfd:TMultipartFormData; Type_Info:PTypeInfo;  key:string;  value:TValue);
       class procedure AddInt64   (var mpfd:TMultipartFormData; Type_Info:PTypeInfo;  key:string;  value:TValue);
       class procedure AddBoolean (var mpfd:TMultipartFormData; Type_Info:PTypeInfo;  key:string;  value:TValue);
       class procedure AddClass_TtgFileToSend(var mpfd:TMultipartFormData; TypeInfo:PTypeInfo; key:string; value:TValue);
     end;

implementation uses TelegAPI.Helpers;

class procedure TtgParamLoader.AddInteger(var mpfd: TMultipartFormData; Type_Info: PTypeInfo; key: string; value: TValue);
begin
  if Value.AsInteger <> 0 then mpfd.AddField(Key, Value.AsInteger.ToString);
end;

class procedure TtgParamLoader.AddString(var mpfd: TMultipartFormData; Type_Info: PTypeInfo; key: string; value: TValue);
begin
  if not value.AsString.IsEmpty then mpfd.AddField(Key, Value.AsString);
end;

class procedure TtgParamLoader.AddInt64(var mpfd: TMultipartFormData; Type_Info: PTypeInfo; key: string; value: TValue);
begin
  if Value.AsInt64 <> 0 then mpfd.AddField(Key, Value.AsInt64.ToString);
end;

class procedure TtgParamLoader.AddBoolean(var mpfd: TMultipartFormData; Type_Info: PTypeInfo; key: string; value: TValue);
begin
  if Value.AsBoolean then mpfd.AddField(Key, Value.AsBoolean.ToString(TUseBoolStrs.True));
end;

class procedure TtgParamLoader.AddClass_TtgFileToSend(var mpfd: TMultipartFormData; TypeInfo: PTypeInfo; key: string; value: TValue);
var fts:TtgFileToSend;
begin
  fts:=Value.AsType<TtgFileToSend>;

  if assigned(fts.Content) then mpfd.AddStream(Key, fts.Content)
    else mpfd.AddFile(Key, fts.FileName);
end;

end.
