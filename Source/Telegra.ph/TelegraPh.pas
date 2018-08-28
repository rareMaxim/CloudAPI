{***************************************************************************}
{                                                                           }
{           CloudApi for Delphi                                             }
{                                                                           }
{           Copyright (c) 2014-2018 Maxim Sysoev                            }
{                                                                           }
{           https://t.me/CloudAPI                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit TelegraPh;

interface

uses
  CloudAPI.BaseComponent,
  TelegraPh.Types;

type
  TTelegraPh = class(TCloudApiBaseComponent)
  protected
    procedure DoInitApiCore; override;
  public

    /// <summary>
    /// Use this method to create a new Telegraph account. Most users only
    /// need one account, but this can be useful for channel administrators
    /// who would like to keep individual author names and profile links for
    /// each of their channels.
    /// </summary>
    /// <param name="ShortName">
    /// Required. Account name, helps users with several accounts remember
    /// which they are currently using. Displayed to the user above the
    /// "Edit/Publish" button on Telegra.ph, other users don't see this name.
    /// </param>
    /// <param name="AuthorName">
    /// Default author name used when creating new articles.
    /// </param>
    /// <param name="AuthorUrl">
    /// Default profile link, opened when users click on the author's name
    /// below the title. Can be any link, not necessarily to a Telegram
    /// profile or channel.
    /// </param>
    /// <returns>
    /// On success, returns an Account object with the regular fields and an
    /// additional access_token field.
    /// </returns>
    function createAccount(const ShortName: string; const AuthorName: string = ''; const AuthorUrl: string = '')
      : IphAccount;
    /// <summary>
    /// Use this method to update information about a Telegraph account. Pass
    /// only the parameters that you want to edit.
    /// </summary>
    /// <param name="AccessToken">
    /// Required. Access token of the Telegraph account.
    /// </param>
    /// <param name="ShortName">
    /// New account name.
    /// </param>
    /// <param name="AuthorName">
    /// New default author name used when creating new articles.
    /// </param>
    /// <param name="AuthorUrl">
    /// New default profile link, opened when users click on the author's
    /// name below the title. Can be any link, not necessarily to a Telegram
    /// profile or channel.
    /// </param>
    /// <returns>
    /// On success, returns an Account object with the default fields.
    /// </returns>
    function editAccountInfo(const AccessToken: string; const ShortName: string = ''; const AuthorName: string = '';
      const AuthorUrl: string = ''): IphAccount;
    /// <summary>
    /// Use this method to get information about a Telegraph account. Returns
    /// an Account object on success.
    /// </summary>
    /// <param name="AccessToken">
    /// Required. Access token of the Telegraph account.
    /// </param>
    function getAccountInfo(const AccessToken: string): IphAccount;
    /// <summary>
    /// Use this method to revoke access_token and generate a new one, for
    /// example, if the user would like to reset all connected sessions, or
    /// you have reasons to believe the token was compromised.
    /// </summary>
    /// <param name="AccessToken">
    /// Required. Access token of the Telegraph account.
    /// </param>
    /// <returns>
    /// On success, returns an Account object with new access_token and
    /// auth_url fields.
    /// </returns>
    function revokeAccessToken(const AccessToken: string): IphAccount;
    /// <summary>
    /// Use this method to create a new Telegraph page.
    /// </summary>
    /// <param name="AccessToken">
    /// Required. Access token of the Telegraph account.
    /// </param>
    /// <param name="title">
    /// Required. Page title.
    /// </param>
    /// <param name="author_name">
    /// Author name, displayed below the article's title.
    /// </param>
    /// <param name="author_url">
    /// Profile link, opened when users click on the author's name below the
    /// title. Can be any link, not necessarily to a Telegram profile or
    /// channel.
    /// </param>
    /// <param name="content">
    /// Required. Content of the page.
    /// </param>
    /// <param name="return_content">
    /// If true, a content field will be returned in the Page object
    /// </param>
    /// <returns>
    /// On success, returns a Page object.
    /// </returns>
    function createPage(const AccessToken: string; const title: string; const author_name: string = '';
      const author_url: string = ''; const content: TphContent = nil; const return_content: Boolean = False): IphPage;
    /// <summary>
    /// Use this method to edit an existing Telegraph page
    /// </summary>
    /// <param name="AccessToken">
    /// Required. Access token of the Telegraph account.
    /// </param>
    /// <param name="path">
    /// Required. Path to the page.
    /// </param>
    /// <param name="title">
    /// Required. Page title.
    /// </param>
    /// <param name="content">
    /// Required. Content of the page.
    /// </param>
    /// <param name="author_name">
    /// Author name, displayed below the article's title.
    /// </param>
    /// <param name="author_url">
    /// Profile link, opened when users click on the author's name below the
    /// title. Can be any link, not necessarily to a Telegram profile or
    /// channel.
    /// </param>
    /// <param name="return_content">
    /// If true, a content field will be returned in the Page object.
    /// </param>
    /// <returns>
    /// On success, returns a Page object.
    /// </returns>
    function editPage(const AccessToken: string; const path: string; const title: string; const content: TphContent;
      const author_name: string = ''; const author_url: string = ''; const return_content: Boolean = False): IphPage;
    /// <summary>
    /// Use this method to get a Telegraph page. Returns a Page object on
    /// success.
    /// </summary>
    /// <param name="path">
    /// Required. Path to the Telegraph page (in the format Title-12-31, i.e.
    /// everything that comes after http://telegra.ph/).
    /// </param>
    /// <param name="return_content">
    /// If true, content field will be returned in Page object.
    /// </param>
    function getPage(const path: string; const return_content: Boolean = False): IphPage;
    /// <summary>
    /// Use this method to get a list of pages belonging to a Telegraph
    /// account. Returns a PageList object, sorted by most recently created
    /// pages first.
    /// </summary>
    /// <param name="access_token">
    /// Required. Access token of the Telegraph account.
    /// </param>
    /// <param name="offset">
    /// Sequential number of the first page to be returned. <br />
    /// </param>
    /// <param name="limit">
    /// Limits the number of pages to be retrieved.
    /// </param>
    function getPageList(const access_token: string; const offset: Integer = 0; const limit: Integer = 50): IphPageList;
    /// <summary>
    /// Use this method to get the number of views for a Telegraph article.
    /// Returns a PageViews object on success. By default, the total number
    /// of page views will be returned.
    /// </summary>
    /// <param name="path">
    /// Required. Path to the Telegraph page (in the format Title-12-31,
    /// where 12 is the month and 31 the day the article was first
    /// published).
    /// </param>
    /// <param name="year">
    /// Required if month is passed. If passed, the number of page views for
    /// the requested year will be returned.
    /// </param>
    /// <param name="month">
    /// Required if day is passed. If passed, the number of page views for
    /// the requested month will be returned.
    /// </param>
    /// <param name="day">
    /// Required if day is passed. If passed, the number of page views for
    /// the requested month will be returned.
    /// </param>
    /// <param name="hour">
    /// If passed, the number of page views for the requested hour will be
    /// returned.
    /// </param>
    function getViews(const path: string; const year: Integer = 0; const month: Integer = 0; const day: Integer = 0;
      const hour: Integer = 0): IphPageViews;
  published
    property Domain;
  end;

implementation

uses
  CloudAPI.Request,
  CloudAPI.Utils.Json,
  CloudAPI.Exception,
  System.SysUtils,
  System.Json;

{ TTelegraPh }

function TTelegraPh.createAccount(const ShortName, AuthorName, AuthorUrl: string): IphAccount;
begin
  Result := TphAccount.Create( //
    GetRequest.SetMethod('createAccount') //
    .AddParameter('short_name', ShortName, '', True) //
    .AddParameter('author_name', AuthorName, '', False) //
    .AddParameter('author_url', AuthorUrl, '', False) //
    .ExecuteAsString);
end;

function TTelegraPh.createPage(const AccessToken, title, author_name, author_url: string; const content: TphContent;
  const return_content: Boolean): IphPage;
begin
  Result := TphPage.Create( //
    GetRequest.SetMethod('createPage') //
    .AddParameter('access_token', AccessToken, '', True) //
    .AddParameter('title', title, '', True) //
    .AddParameter('author_name', author_name, '', False) //
    .AddParameter('author_url', author_url, '', False) //
    .AddParameter('content', content.ToJson, TphContent(nil).ToJson, False) //
    .AddParameter('return_content', return_content, False, False) //
    .ExecuteAsString);
end;

procedure TTelegraPh.DoInitApiCore;
begin
  inherited;
  Domain := 'https://api.telegra.ph/';
  GetRequest.StoreAutoFormat := TStoreFormat.InUrl;
  GetRequest.OnDataReceiveAsString := function(AInput: string): string
    var
      LJSON: TJSONObject;
      LExcText: string;
    begin
      Result := '';
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        if LJSON.GetValue('ok') is TJSONFalse then
        begin
          LExcText := (LJSON.GetValue('error') as TJSONString).Value;
          DoCallLogEvent(ECloudApiException.Create(LExcText, ''), True);
        end
        else
          Result := LJSON.GetValue('result').ToString;
      finally
        LJSON.Free;
      end;
    end;
end;

function TTelegraPh.editAccountInfo(const AccessToken, ShortName, AuthorName, AuthorUrl: string): IphAccount;
begin
  Result := TphAccount.Create( //
    GetRequest.SetMethod('editAccountInfo') //
    .AddParameter('access_token', AccessToken, '', True) //
    .AddParameter('short_name', ShortName, '', False) //
    .AddParameter('author_name', AuthorName, '', False) //
    .AddParameter('author_url', AuthorUrl, '', False) //
    .ExecuteAsString);
end;

function TTelegraPh.editPage(const AccessToken, path, title: string; const content: TphContent;
  const author_name, author_url: string; const return_content: Boolean): IphPage;
begin
  Result := TphPage.Create( //
    GetRequest.SetMethod('editPage') //
    .AddParameter('access_token', AccessToken, '', True) //
    .AddParameter('path', path, '', True) //
    .AddParameter('title', title, '', True) //
    .AddParameter('content', content.ToJson, TphContent(nil).ToJson, True) //
    .AddParameter('author_name', author_name, '', False) //
    .AddParameter('author_url', author_url, '', False) //
    .AddParameter('return_content', return_content, False, False) //
    .ExecuteAsString);
end;

function TTelegraPh.getAccountInfo(const AccessToken: string): IphAccount;
begin
  Result := TphAccount.Create( //
    GetRequest.SetMethod('getAccountInfo') //
    .AddParameter('access_token', AccessToken, '', True) //
    .AddParameter('fields', '["short_name","author_name","author_url", "auth_url", "page_count"]', '', False) //
    .ExecuteAsString);
end;

function TTelegraPh.getPage(const path: string; const return_content: Boolean): IphPage;
begin
  Result := TphPage.Create( //
    GetRequest.SetMethod('getPage') //
    .AddParameter('path', path, '', True) //
    .AddParameter('return_content', return_content, False, False) //
    .ExecuteAsString);
end;

function TTelegraPh.getPageList(const access_token: string; const offset, limit: Integer): IphPageList;
begin
  Result := TphPageList.Create( //
    GetRequest.SetMethod('getPageList') //
    .AddParameter('access_token', access_token, '', True) //
    .AddParameter('offset', offset, 0, False) //
    .AddParameter('limit', limit, 50, False) //
    .ExecuteAsString);
end;

function TTelegraPh.getViews(const path: string; const year, month, day, hour: Integer): IphPageViews;
begin
  Result := TphPageViews.Create( //
    GetRequest.SetMethod('getViews') //
    .AddParameter('path', path, '', True) //
    .AddParameter('year', year, 0, False) //
    .AddParameter('month', month, 0, False) //
    .AddParameter('day', day, 0, False) //
    .AddParameter('hour', hour, 0, False) //
    .ExecuteAsString);
end;

function TTelegraPh.revokeAccessToken(const AccessToken: string): IphAccount;
begin
  Result := TphAccount.Create( //
    GetRequest.SetMethod('revokeAccessToken') //
    .AddParameter('access_token', AccessToken, '', True) //
    .ExecuteAsString);
end;

end.
