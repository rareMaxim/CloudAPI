unit TelegraPh;

interface

uses
  TelegraPh.Types,
  TelegAPI.Bot.Impl;

type
  TTelegraPh = class(TTelegramBotBase)
  protected
    procedure DoInitApiCore; override;
  public
    function createAccount(const ShortName: string; const AuthorName: string =
      ''; const AuthorUrl: string = ''): IphAccount;
    function editAccountInfo(const AccessToken: string; const ShortName: string
      = ''; const AuthorName: string = ''; const AuthorUrl: string = ''): IphAccount;
    function getAccountInfo(const AccessToken: string): IphAccount;
    function revokeAccessToken(const AccessToken: string): IphAccount;
    function createPage(const AccessToken: string; const title: string; const
      author_name: string = ''; const author_url: string = ''; const content:
      TphContent = nil; const return_content: Boolean = False): IphPage;
    function editPage(const AccessToken: string; const path: string; const title:
      string; const content: TphContent; const author_name: string = ''; const
      author_url: string = ''; const return_content: Boolean = False): IphPage;
    function getPage(const path: string; const return_content: Boolean = False): IphPage;
    function getPageList(const access_token: string; const offset: Integer = 0;
      const limit: Integer = 50): IphPageList;
    function getViews(const path: string; const year: Integer = 0; const month:
      Integer = 0; const day: Integer = 0; const hour: Integer = 0): IphPageViews;
  published
    property UrlAPI;
  end;

implementation

uses
  TelegAPI.Utils.Json,
  System.SysUtils,
  System.JSON;

{ TTelegraPh }

function TTelegraPh.createAccount(const ShortName, AuthorName, AuthorUrl: string):
  IphAccount;
begin
  Result := TphAccount.Create(//
    GetRequest.SetMethod('createAccount')//
    .AddParameter('short_name', ShortName, '', True)//
    .AddParameter('author_name', AuthorName, '', False)//
    .AddParameter('author_url', AuthorUrl, '', False)//
    .Execute);
end;

function TTelegraPh.createPage(const AccessToken, title, author_name, author_url:
  string; const content: TphContent; const return_content: Boolean): IphPage;
begin
  Result := TphPage.Create(//
    GetRequest.SetMethod('createPage')//
    .AddParameter('access_token', AccessToken, '', True)//
    .AddParameter('title', title, '', True)//
    .AddParameter('author_name', author_name, '', False)//
    .AddParameter('author_url', author_url, '', False)//
    .AddParameter('content', content.ToJson, TphContent(nil).ToJson, False)//
    .AddParameter('return_content', return_content, False, False)//
    .Execute);
end;

procedure TTelegraPh.DoInitApiCore;
begin
  inherited;
  UrlAPI := 'https://api.telegra.ph/';
  GetRequest.DataExtractor :=
    function(AInput: string): string
    var
      LJSON: TJSONObject;
    begin
      Result := '';
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        if LJSON.GetValue('ok') is TJSONFalse then
          Logger.Error((LJSON.GetValue('error') as TJSONString).Value)
        else
          Result := LJSON.GetValue('result').ToString;
      finally
        LJSON.Free;
      end;
    end;
end;

function TTelegraPh.editAccountInfo(const AccessToken, ShortName, AuthorName,
  AuthorUrl: string): IphAccount;
begin
  Result := TphAccount.Create(//
    GetRequest.SetMethod('editAccountInfo')//
    .AddParameter('access_token', AccessToken, '', True)//
    .AddParameter('short_name', ShortName, '', False)//
    .AddParameter('author_name', AuthorName, '', False)//
    .AddParameter('author_url', AuthorUrl, '', False)//
    .Execute);
end;

function TTelegraPh.editPage(const AccessToken, Path, title: string; const
  content: TphContent; const author_name, author_url: string; const
  return_content: Boolean): IphPage;
begin
  Result := TphPage.Create(//
    GetRequest.SetMethod('editPage')//
    .AddParameter('access_token', AccessToken, '', True)//
    .AddParameter('path', Path, '', True)//
    .AddParameter('title', title, '', True)//
    .AddParameter('content', content.ToJson, TphContent(nil).ToJson, True)//
    .AddParameter('author_name', author_name, '', False)//
    .AddParameter('author_url', author_url, '', False)//
    .AddParameter('return_content', return_content, False, False)//
    .Execute);
end;

function TTelegraPh.getAccountInfo(const AccessToken: string): IphAccount;
begin
  Result := TphAccount.Create(//
    GetRequest.SetMethod('getAccountInfo')//
    .AddParameter('access_token', AccessToken, '', True)//
    .AddParameter('fields',
    '["short_name","author_name","author_url", "auth_url", "page_count"]', '', False)//
    .Execute);
end;

function TTelegraPh.getPage(const path: string; const return_content: Boolean): IphPage;
begin
  Result := TphPage.Create(//
    GetRequest.SetMethod('getPage')//
    .AddParameter('path', path, '', True)//
    .AddParameter('return_content', return_content, False, False)//
    .Execute);
end;

function TTelegraPh.getPageList(const access_token: string; const offset, limit:
  Integer): IphPageList;
begin
  Result := TphPageList.Create(//
    GetRequest.SetMethod('getPageList')//
    .AddParameter('access_token', access_token, '', True)//
    .AddParameter('offset', offset, 0, False)//
    .AddParameter('limit', limit, 50, False)//
    .Execute);
end;

function TTelegraPh.getViews(const path: string; const year, month, day, hour:
  Integer): IphPageViews;
begin
  Result := TphPageViews.Create(//
    GetRequest.SetMethod('getViews')//
    .AddParameter('path', path, '', True)//
    .AddParameter('year', year, 0, False)//
    .AddParameter('month', month, 0, False)//
    .AddParameter('day', day, 0, false)//
    .AddParameter('hour', hour, 0, False)//
    .Execute);
end;

function TTelegraPh.revokeAccessToken(const AccessToken: string): IphAccount;
begin
  Result := TphAccount.Create(//
    GetRequest.SetMethod('revokeAccessToken')//
    .AddParameter('access_token', AccessToken, '', True)//
    .Execute);
end;

end.

