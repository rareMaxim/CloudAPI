unit TelegaPi.Types.InputMessageContents;

interface

uses
  XSuperObject;

type

  /// <summary>
  ///   This object represents the content of a message to be sent as a result
  ///   of an inline query.
  /// </summary>
  [Alias('InputMessageContent')]
  TtgInputMessageContent = class
  end;

  /// <summary>
  ///   Represents the content of a contact message to be sent as the result of
  ///   an inline query.
  /// </summary>
  [Alias('InputContactMessageContent')]
  TtgInputContactMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Contact's phone number
    /// </summary>
    [Alias('phone_number')]
    PhoneNumber: string;
    /// <summary>
    ///   Contact's first name
    /// </summary>
    [Alias('first_name')]
    FirstName: string;
    /// <summary>
    ///   Optional. Contact's last name
    /// </summary>
    [Alias('last_name')]
    LastName: string;
  end;

  /// <summary>
  ///   Represents the content of a location message to be sent as the result
  ///   of an inline query.
  /// </summary>
  [Alias('InputLocationMessageContent')]
  TtgInputLocationMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Latitude of the location in degrees
    /// </summary>
    [Alias('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Longitude of the location in degrees
    /// </summary>
    [Alias('longitude')]
    Longitude: Single;
  end;

  /// <summary>
  ///   Represents the content of a text message to be sent as the result of an
  ///   inline query.
  /// </summary>
  [Alias('InputTextMessageContent')]
  TtgInputTextMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Text of the message to be sent, 1-4096 characters
    /// </summary>
    [Alias('message_text')]
    MessageText: string;
    /// <summary>
    ///   Optional. Send Markdown or HTML, if you want Telegram apps to show
    ///   bold, italic, fixed-width text or inline URLs in your bot's message.
    /// </summary>
    [Alias('parse_mode')]
    ParseMode: string;
    /// <summary>
    ///   Optional. Disables link previews for links in the sent message
    /// </summary>
    [Alias('disable_web_page_preview')]
    DisableWebPagePreview: Boolean;
  end;

  /// <summary>
  ///   Represents the content of a venue message to be sent as the result of
  ///   an inline query.
  /// </summary>
  [Alias('InputVenueMessageContent')]
  TtgInputVenueMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Latitude of the venue in degrees
    /// </summary>
    [Alias('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Longitude of the venue in degrees
    /// </summary>
    [Alias('longitude')]
    Longitude: Single;
    /// <summary>
    ///   Name of the venue
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   Address of the venue
    /// </summary>
    [Alias('address')]
    Address: string;
    /// <summary>
    ///   Optional. Foursquare identifier of the venue, if known
    /// </summary>
    [Alias('foursquare_id')]
    FoursquareId: string;
  end;

implementation

end.

