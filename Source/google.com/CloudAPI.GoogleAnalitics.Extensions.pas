unit CloudAPI.GoogleAnalitics.Extensions;

interface

uses
  System.Classes,
  System.Types;

type
  {$SCOPEDENUMS ON}
  TgaTypeTreatment = (pageview, screenview, event, transaction, item, social,
    Exception, timing);

  TgaSessionControllerState = (Empty, Start, &End);
  {$SCOPEDENUMS OFF}

  TgaExtension = class(TPersistent)
  public
    procedure FillData(const HitType: string; ADataStorage: TStringList);
      virtual; abstract;
  end;


  /// <summary>
  /// Общие поля
  /// </summary>
  TgaGeneral = class(TgaExtension)
  private
    FVersion: string;
    FTrackingId: string;
    FDataSource: string;
    FAnonymizingIP: Boolean;
    FQueueTime: Integer;
    FCacheLock: string;
  public
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  public
    constructor Create;
  published
    /// <summary>
    /// Версия протокола
    /// </summary>
    /// <remarks>
    /// Является обязательным для всех типов обращений.
    /// Версия протокола. Текущее значение – 1.
    /// Оно изменится только в том случае, если в протокол будут внесены изменения без обратной совместимости.
    /// </remarks>
    property Version: string read FVersion;
    /// <summary>
    /// Идентификатор отслеживания/идентификатор веб-ресурса
    /// </summary>
    /// <remarks>
    /// Является обязательным для всех типов обращений.
    /// Идентификатор отслеживания/идентификатор веб-ресурса в формате <c>UA-XXXX-Y</c>. С этим
    /// идентификатором связываются все собираемые данные.
    /// </remarks>
    property TrackingId: string read FTrackingId write FTrackingId;
    /// <summary>
    /// Источник данных
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// Указывает источник данных для обращения.
    /// Обращения от analytics.js будут иметь значение web, обращения от мобильных SDK – значение app.
    /// </remarks>
    property DataSource: string read FDataSource write FDataSource;
    /// <summary>
    /// Анонимизация IP
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// IP-адрес отправителя (при наличии) переводится в анонимную форму.
    /// </remarks>
    property AnonymizingIP: Boolean read FAnonymizingIP write FAnonymizingIP;
    /// <summary>
    /// Время в очереди
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// Используется для сбора офлайн-обращений (латентных обращений). Значение представляет собой временную дельту (в миллисекундах) между моментом, когда произошло обращение, и его отправкой. Значение должно быть больше или равно 0. Если значение превышает четыре часа, обращение может быть не обработано.
    /// </remarks>
    property QueueTime: Integer read FQueueTime write FQueueTime;
    /// <summary>
    /// Блокировка кеша
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// Используется для отправки случайно выбранных значений в запросах GET, чтобы браузеры и прокси-серверы не кешировали обращения. Этот параметр должен быть последним, так как, по нашему опыту, некоторые сторонние программы фильтрации неправильно добавляют дополнительные параметры в HTTP-запросы. Это значение не попадает в отчеты. Это значение не попадает в отчеты.
    /// </remarks>
    property CacheLock: string read FCacheLock write FCacheLock;
  end;
  /// <summary>
  /// Пользователь
  /// </summary>

  TgaUser = class(TgaExtension)
  private
    FClientID: string;
    FUserID: string;
  public
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    /// Идентификатор клиента
    /// </summary>
    /// <remarks>
    /// Это поле должно быть заполнено, если в запросе не указан User ID (uid). С помощью значения этого поля выполняется анонимная идентификация экземпляра пользователя, устройства или браузера. На сайтах этот идентификатор обычно сохраняется в основном файле cookie, который действует два года, а в случае мобильных приложений – случайным образом генерируется для каждого экземпляра установленного приложения. В этом поле должен быть указан универсальный уникальный идентификатор (версии 4), как описано в файле http://www.ietf.org/rfc/rfc4122.txt.
    /// </remarks>
    property ClientID: string read FClientID write FClientID;
    /// <summary>
    /// Идентификатор пользователя
    /// </summary>
    /// <remarks>
    /// Это поле обязательно, если в запросе не задан идентификатор клиента. User ID – известный идентификатор, присваиваемый пользователю владельцем сайта или пользователем библиотеки отслеживания. Он должен быть анонимным, т. е. не связанным с личной информацией, а его значение не должно сохраняться с помощью файлов cookie или каких-либо других средств хранения данных в Google Analytics.
    /// </remarks>
    property UserID: string read FUserID write FUserID;
  end;

  /// <summary>
  /// Сеанс
  /// </summary>
  TgaSession = class(TgaExtension)
  private
    FSessionController: TgaSessionControllerState;
    FUserAgent: string;
    FIpOverride: string;
    FGeoID: string;
  protected
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    /// Контроллер сеансов
    /// </summary>
    /// <remarks>
    /// Используется для контроля за продолжительностью сеанса. При значении start с этого обращения начинается новый сеанс, а при значении end на этом обращении заканчивается текущий сеанс. Все остальные значения игнорируются.
    /// </remarks>
    property SessionController: TgaSessionControllerState read
      FSessionController write FSessionController;
    /// <summary>
    /// Переопределение IP
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// IP-адрес пользователя. Этот IP-адрес должен быть действительным, в формате IPv4 или IPv6. IP пользователей всегда анонимизируются.
    /// </remarks>
    property IpOverride: string read FIpOverride write FIpOverride;
    /// <summary>
    /// Переопределение агента пользователя
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// Агент пользователя браузера. Обратите внимание, что Google предоставляет библиотеки, позволяющие идентифицировать действительных агентов пользователя. Использование собственного агента ненадежно и может привести к ошибкам.
    /// </remarks>
    property UserAgent: string read FUserAgent write FUserAgent;
    /// <summary>
    /// Переопределение геоданных
    /// </summary>
    /// <remarks>
    /// Необязательное поле.
    /// Географическое местоположение пользователя. Идентификатор местоположения должен представлять собой двухбуквенный код страны или идентификатор критерия для города или региона (см. http://developers.google.com/analytics/devguides/collection/protocol/v1/geoid). Этот параметр переопределяет все значения, определенные по IP-адресу, в том числе параметр переопределения IP. При неверном коде географические параметры будут иметь значение (not set).
    /// </remarks>
    property GeoID: string read FGeoID write FGeoID;
  end;
  /// <summary>
  /// Источники трафика
  /// </summary>

  TgaTrafficSources = class(TgaExtension)
  private
    FDocumentReferrer: string;
    FCampaignName: string;
    FCampaignSource: string;
    FCampaignMedium: string;
    FCampaignKeyword: string;
    FCampaignContent: string;
    FCampaignID: string;
    FGoogleAdWordsID: string;
    FGoogleDisplayAdsID: string;
  public
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    ///  URL перехода к документу
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Показывает, с какого URL поступил трафик на сайт. Это значение используется для определения источника трафика. Формат значения – URL.
    /// </remarks>
    property DocumentReferrer: string read FDocumentReferrer write FDocumentReferrer;
    /// <summary>
    ///  Название кампании
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает название кампании.
    /// </remarks>
    property CampaignName: string read FCampaignName write FCampaignName;
    /// <summary>
    ///  Источник кампании
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает источник кампании.
    /// </remarks>
    property CampaignSource: string read FCampaignSource write FCampaignSource;
    /// <summary>
    ///  Канал кампании
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает канал кампании.
    /// </remarks>
    property CampaignMedium: string read FCampaignMedium write FCampaignMedium;
    /// <summary>
    ///  Ключевое слово кампании
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет ключевое слово кампании.
    /// </remarks>
    property CampaignKeyword: string read FCampaignKeyword write FCampaignKeyword;
    /// <summary>
    ///  Содержание кампании
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет содержание кампании.
    /// </remarks>
    property CampaignContent: string read FCampaignContent write FCampaignContent;
    /// <summary>
    ///  Идентификатор кампании
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет идентификатор кампании.
    /// </remarks>
    property CampaignID: string read FCampaignID write FCampaignID;
    /// <summary>
    ///  Идентификатор Google Рекламы
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет идентификатор Google Рекламы.
    /// </remarks>
    property GoogleAdWordsID: string read FGoogleAdWordsID write FGoogleAdWordsID;
    /// <summary>
    ///  Идентификатор медийных объявлений Google
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает идентификатор медийных объявлений Google.
    /// </remarks>
    property GoogleDisplayAdsID: string read FGoogleDisplayAdsID write
      FGoogleDisplayAdsID;
  end;

  /// <summary>
  ///   Информация о системе
  /// </summary>
  TgaSystem = class(TgaExtension)
  private
    FScreenResolution: TSizeF;
    FViewportSize: TSizeF;
    FDocumentEncoding: string;
    FScreenColors: string;
    FUserLanguage: string;
    FJavaEnabled: Boolean;
    FFlashVersion: string;
  protected
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    ///  Разрешение экрана
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает разрешение экрана.
    /// </remarks>
    property ScreenResolution: TSizeF read FScreenResolution write FScreenResolution;
    /// <summary>
    ///  Окно просмотра
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет размер видимой области браузера/устройства.
    /// </remarks>
    property ViewportSize: TSizeF read FViewportSize write FViewportSize;
    /// <summary>
    ///  Кодирование документа
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет набор символов для кодирования страницы/документа.
    /// </remarks>
    property DocumentEncoding: string read FDocumentEncoding write FDocumentEncoding;
    /// <summary>
    ///  Цвета экрана
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет глубину цветов экрана.
    /// </remarks>
    property ScreenColors: string read FScreenColors write FScreenColors;
    /// <summary>
    ///  Язык пользователя
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Определяет язык пользователя.
    /// </remarks>
    property UserLanguage: string read FUserLanguage write FUserLanguage;
    /// <summary>
    ///   Поддержка Java включена
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает, включена ли поддержка Java.
    /// </remarks>
    property JavaEnabled: Boolean read FJavaEnabled write FJavaEnabled;
    /// <summary>
    ///   Версия Flash
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает версию Flash.
    /// </remarks>
    property FlashVersion: string read FFlashVersion write FFlashVersion;
  end;


  /// <summary>
  ///   Обращение
  /// </summary>
  TgaHit = class(TgaExtension)
  private
    FHitType: TgaTypeTreatment;
    FNonInteractionHit: Boolean;
  protected
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  public
    constructor Create;
  published
    /// <summary>
    ///   Тип обращения
    /// </summary>
    /// <remarks>
    ///   Является обязательным для всех типов обращений.
    ///  Тип обращения. Возможные варианты: pageview, screenview, event, transaction, item, social, exception, timing.
    /// </remarks>
    property HitType: TgaTypeTreatment read FHitType write FHitType;
    /// <summary>
    ///   Пассивное событие
    /// </summary>
    /// <remarks>
    ///   Необязательное поле.
    ///   Указывает на то, что обращение не должно считаться взаимодействием.
    /// </remarks>
    property NonInteractionHit: Boolean read FNonInteractionHit write
      FNonInteractionHit default False;
  end;


  /// <summary>
  ///  Информация о содержании
  /// </summary>
  TgaContentInformation = class(TgaExtension)
  private
    FDocumentPath: string;
    FDocumentLocationURL: string;
    FDocumentHostName: string;
    FDocumentTitle: string;
    FScreenName: string;
    FContentGroup: string;
    FLinkID: string;
  protected
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    ///   URL местоположения документа
    /// </summary>
    /// <remarks>
    ///   Необязательное поле.
    ///  Этот параметр позволяет передать полный URL (путь к документу) страницы, на которой размещен контент. Переопределить имя хоста и путь+запрос можно с помощью параметров dh и dp соответственно. Клиенты JavaScript определяют URL, используя комбинацию параметров браузера document.location.origin, document.location.pathname и document.location.search. Если в URL есть какая-либо личная информация, включая сведения, необходимые для аутентификации, ее нужно удалить. Для обращений типа pageview необходимо указывать либо параметр dl, либо параметры dh и dp одновременно.
    /// </remarks>
    property DocumentLocationURL: string read FDocumentLocationURL write
      FDocumentLocationURL;
    /// <summary>
    ///  Имя хоста документа
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает имя хоста, где хранится контент.
    /// </remarks>
    property DocumentHostName: string read FDocumentHostName write FDocumentHostName;
    /// <summary>
    ///   Путь к документу
    /// </summary>
    /// <remarks>
    ///    Необязательное поле.
    ///  Часть URL страницы, определяющая путь. Путь должен начинаться с символа косой черты (/). Для обращений типа pageview необходимо указать либо параметр dl, либо одновременно параметры dh и dp.
    /// </remarks>
    property DocumentPath: string read FDocumentPath write FDocumentPath;
    /// <summary>
    ///  Заголовок документа
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Заголовок страницы/документа.
    /// </remarks>
    property DocumentTitle: string read FDocumentTitle write FDocumentTitle;
    /// <summary>
    ///   Название экрана
    /// </summary>
    /// <remarks>
    ///    Обязательно для обращений типа screenview (просмотр экрана).
    ///  Это необязательный параметр для веб-ресурсов и обязательный – для мобильных ресурсов. Он используется в качестве названия экрана для обращений типа screenview (просмотр экрана). Для веб-ресурсов по умолчанию используется уникальный URL страницы (параметр dl "как есть" либо комбинация параметров dh и dp).
    /// </remarks>
    property ScreenName: string read FScreenName write FScreenName;
    /// <summary>
    ///   Группа контента (НЕ ПОДДЕРЖИВАЕТСЯ НА ДАННЫЙ МОМЕНТ)
    /// </summary>
    /// <remarks>
    ///   Необязательное поле.
    ///   У вас может быть до пяти групп контента, каждой из которых присваивается номер от 1 до 5. Каждая из них в свою очередь может включать до 100 групп контента. Значением параметра "Группа контента" должен быть текст, обозначающий категорию контента в иерархической структуре. При этом в качестве разделителя используется косая черта "/". Если такие знаки содержатся в начале или конце строки, они будут удалены. Если этот знак указан несколько раз подряд, то его повторы удаляются. Например, строка "/a//b/" преобразуется в "a/b".
    /// </remarks>
    property ContentGroup: string read FContentGroup write FContentGroup;
    /// <summary>
    ///   Идентификатор ссылки
    /// </summary>
    /// <remarks>
    ///   Необязательное поле.
    ///  Идентификатор элемента DOM, на который нажал пользователь. Его значение используется для различения ссылок на один URL в отчетах "Статистика страницы" в случаях, когда для ресурса включена улучшенная атрибуция ссылок.
    /// </remarks>
    property LinkID: string read FLinkID write FLinkID;
  end;

 /// <summary>
 ///   Отслеживание приложений
 /// </summary>
  TgaAppTracking = class(TgaExtension)
  private
    FApplicationName: string;
    FApplicationID: string;
    FApplicationVersion: string;
    FApplicationInstallerID: string;
  protected
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    ///  Название приложения
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает название приложения. Это поле является обязательным для обращений, в которых содержатся данные, связанные с приложением (например, версия приложения, его идентификатор или идентификатор установщика). Для веб-ресурсов необязательно.
    /// </remarks>
    property ApplicationName: string read FApplicationName write FApplicationName;
    /// <summary>
    ///   Идентификатор приложения
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Идентификатор приложения.
    /// </remarks>
    property ApplicationID: string read FApplicationID write FApplicationID;
    /// <summary>
    ///  Версия приложения
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Указывает версию приложения.
    /// </remarks>
    property ApplicationVersion: string read FApplicationVersion write
      FApplicationVersion;
    /// <summary>
    ///  Идентификатор установщика приложения
    /// </summary>
    /// <remarks>
    ///  Необязательное поле.
    ///  Идентификатор установщика приложения.
    /// </remarks>
    property ApplicationInstallerID: string read FApplicationInstallerID write
      FApplicationInstallerID;
  end;

  TgaException = class(TgaExtension)
  private
    FText: string;
    FIsFatal: Boolean;
  protected
    procedure FillData(const HitType: string; ADataStorage: TStringList); override;
  published
    /// <summary>
    ///
    /// </summary>
    /// <remarks>
    ///
    /// </remarks>
    property Text: string read FText write FText;
    /// <summary>
    ///
    /// </summary>
    /// <remarks>
    ///
    /// </remarks>
    property IsFatal: Boolean read FIsFatal write FIsFatal;
  end;

implementation

uses
  System.SysUtils;

type
  TgaTypeTreatmentHelper = record helper for TgaTypeTreatment
    function ToString: string;
  end;

  TSizeFHelper = record helper for TSizeF
    function ToString: string;
  end;

  TgaSessionControllerStateHelper = record helper for TgaSessionControllerState
    function ToString: string;
  end;

{ TggTypeTreatmentHelper }

function TgaTypeTreatmentHelper.ToString: string;
begin
  case Self of
    TgaTypeTreatment.pageview:
      Result := 'pageview';
    TgaTypeTreatment.screenview:
      Result := 'screenview';
    TgaTypeTreatment.event:
      Result := 'event';
    TgaTypeTreatment.transaction:
      Result := 'transaction';
    TgaTypeTreatment.item:
      Result := 'item';
    TgaTypeTreatment.social:
      Result := 'social';
    TgaTypeTreatment.Exception:
      Result := 'exception';
    TgaTypeTreatment.timing:
      Result := 'timing';
  end;
end;

{ TSizeHelper }

function TSizeFHelper.ToString: string;
begin
  Result := Height.ToString + 'x' + Width.ToString;
end;
{ TgaUser }

procedure TgaUser.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if (ClientID.IsEmpty) and UserID.IsEmpty then
    raise EArgumentException.Create('A value is required for parameter ClientID or UserID');
  if not ClientID.IsEmpty then
    ADataStorage.AddPair('cid', ClientID);
  if not UserID.IsEmpty then
    ADataStorage.AddPair('uid', UserID);
end;

{ TgaSystem }

procedure TgaSystem.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if not ScreenResolution.IsZero then
    ADataStorage.AddPair('sr', ScreenResolution.ToString);
  if not ViewportSize.IsZero then
    ADataStorage.AddPair('vp', ViewportSize.ToString);
  if not DocumentEncoding.IsEmpty then
    ADataStorage.AddPair('de', DocumentEncoding);
  if not ScreenColors.IsEmpty then
    ADataStorage.AddPair('sd', ScreenColors);
  if not UserLanguage.IsEmpty then
    ADataStorage.AddPair('ul', UserLanguage);
  ADataStorage.AddPair('je', JavaEnabled.ToString);
  if not FlashVersion.IsEmpty then
    ADataStorage.AddPair('fl', FlashVersion);
end;

{ TgaBase }

constructor TgaGeneral.Create;
begin
  inherited;
  FVersion := '1';
  FAnonymizingIP := False;
end;

procedure TgaGeneral.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if not Version.IsEmpty then
    ADataStorage.AddPair('v', Version);
  if not TrackingId.IsEmpty then
    ADataStorage.AddPair('tid', TrackingId);
  if AnonymizingIP then
    ADataStorage.AddPair('aip', '1');
  if not DataSource.IsEmpty then
    ADataStorage.AddPair('ds', DataSource);
  if QueueTime > 0 then
    ADataStorage.AddPair('qt', QueueTime.ToString);
  if not CacheLock.IsEmpty then
    ADataStorage.AddPair('z', CacheLock);
end;

{ TgaSessionController }

procedure TgaSession.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if not UserAgent.IsEmpty then
    ADataStorage.AddPair('ua', UserAgent);
  if not IpOverride.IsEmpty then
    ADataStorage.AddPair('uip', IpOverride);
  if SessionController <> TgaSessionControllerState.Empty then
    ADataStorage.AddPair('sc', SessionController.ToString);
  if not GeoID.IsEmpty then
    ADataStorage.AddPair('geoid', GeoID);
end;

{ TgaTgaSessionControllerStateHelper }

function TgaSessionControllerStateHelper.ToString: string;
begin
  case Self of
    TgaSessionControllerState.Empty:
      Result := '';
    TgaSessionControllerState.Start:
      Result := 'start';
    TgaSessionControllerState.&End:
      Result := 'end';
  end;
end;

{ TgaContentInformation }

procedure TgaContentInformation.FillData(const HitType: string; ADataStorage:
  TStringList);
begin
  if not DocumentLocationURL.IsEmpty then
    ADataStorage.AddPair('dl', DocumentLocationURL);
  if not DocumentHostName.IsEmpty then
    ADataStorage.AddPair('dh', DocumentHostName);
  if not DocumentPath.IsEmpty then
    ADataStorage.AddPair('dp', DocumentPath);
  if not DocumentTitle.IsEmpty then
    ADataStorage.AddPair('dt', DocumentTitle);
  if not ScreenName.IsEmpty then
    ADataStorage.AddPair('cd', ScreenName);
// if not ContentGroup.IsEmpty then
 //   ADataStorage.AddPair('cd', ContentGroup);
  if not LinkID.IsEmpty then
    ADataStorage.AddPair('linkid', LinkID);
end;

{ TgaException }

procedure TgaException.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if (not Text.IsEmpty) and (HitType = 'exception') then
    ADataStorage.AddPair('exd', Text).AddPair('exf', IsFatal.ToInteger.ToString);
end;

{ TgaTrafficSources }

procedure TgaTrafficSources.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if not DocumentReferrer.IsEmpty then
    ADataStorage.AddPair('dr', DocumentReferrer);
  if not CampaignName.IsEmpty then
    ADataStorage.AddPair('cn', CampaignName);
  if not CampaignSource.IsEmpty then
    ADataStorage.AddPair('cs', CampaignSource);
  if not CampaignMedium.IsEmpty then
    ADataStorage.AddPair('cm', CampaignMedium);
  if not CampaignKeyword.IsEmpty then
    ADataStorage.AddPair('ck', CampaignKeyword);
  if not CampaignContent.IsEmpty then
    ADataStorage.AddPair('cc', CampaignContent);
  if not CampaignID.IsEmpty then
    ADataStorage.AddPair('ci', CampaignID);
  if not GoogleAdWordsID.IsEmpty then
    ADataStorage.AddPair('gclid', GoogleAdWordsID);
  if not GoogleDisplayAdsID.IsEmpty then
    ADataStorage.AddPair('dclid', GoogleDisplayAdsID);
end;

{ TgaHit }

constructor TgaHit.Create;
begin
  FNonInteractionHit := False;
end;

procedure TgaHit.FillData(const HitType: string; ADataStorage: TStringList);
begin
  ADataStorage.AddPair('t', Self.HitType.ToString);
  if NonInteractionHit then
    ADataStorage.AddPair('ni', '1');
end;

{ TgaAppTracking }

procedure TgaAppTracking.FillData(const HitType: string; ADataStorage: TStringList);
begin
  if not ApplicationName.IsEmpty then
    ADataStorage.AddPair('an', ApplicationName);
  if not ApplicationID.IsEmpty then
    ADataStorage.AddPair('aid', ApplicationID);
  if not ApplicationVersion.IsEmpty then
    ADataStorage.AddPair('av', ApplicationVersion);
  if not ApplicationInstallerID.IsEmpty then
    ADataStorage.AddPair('aiid', ApplicationInstallerID);
end;

end.

