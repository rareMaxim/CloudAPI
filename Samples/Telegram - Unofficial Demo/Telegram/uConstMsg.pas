unit uConstMsg;

interface

Resourcestring


//Const
  DUH = 'О, дух компьютера, прими эту программу!';

  constAppName                          = 'Сканфильтр-Антипосредник';
  constSiteName                         = 'www.kvartal.pro';
  constURLForum                         = 'https://forum.makeit-team.com/';
  StrSiteName                           = 'https://makeit-team.com';
  constKeyboardLayout                   = '00000419';

  constFiletForExportcxGrid             = 'MS Excel(*.xlsx)|*.xlsx|XML Файл (*.xml)|*.xml|Веб-страница(*.html)|*.html|Текстовый файл(*.txt)|*.txt';
  constHIDKey                           =  'КЛЮЧ "HID"';
  constHIDKeySend                       = 'Отправьте "HID" разработчикам';
  constNoconnectionToDB                 = 'Нет подключения к базе данных программы Квартал ПРО!';
  constError                            = 'Ошибка';
  constErrWhenSearchingPhone            = 'Ошибка при поиске телефона в базе Квартал ПРО: ';
  constErrWhenSearchingPhone2           = 'OK - продолжить, Отмена - остановить';
  constDemo50Rows                       = 'Демо версия обрабатывает 50 строк исходного файла';
  constErrSearchConnectIni              = 'Не найден файл connect.ini в папке программы Квартал ПРО.' + sLineBreak +
                                            'Перед началом работы запустите на этом ПК программу Квартал ПРО' + sLineBreak +
                                            'для настройки соединения с базой данных.' + sLineBreak +
                                            'Или обратитесь в технической поддержку за помощью.';
  constAttention                        = 'Внимание';
  constErrSearchKvxDB                   = 'Не найден путь к базе данных программы Квартал ПРО.'+sLineBreak+
                                            'Перед началом работы запустите на этом компьютере программу Квартал ПРО'+sLineBreak+
                                            'для настройки базы данных.'+sLineBreak+
                                            'Или обратитесь в службу технической поддержки за помощью.';
  constDel1                             = 'Будут удалены объявления, у которых поле "Найдено в базе" НЕ ПУСТОЕ.' + sLineBreak + 'Продолжить?';
  constDel2                             = 'Будут удалены только те объявления, у которых поле "Посредник" НЕ ПУСТОЕ.' + sLineBreak + 'Продолжить?';
  constDelSelectedRow                   = 'Удалить выделенную строку?';
  constDelSelectedRows                  = 'Удалить выделенные строки?';
  constRewriteconfig                    = 'Перезаписать существующую конфигурацию?';
  constParamsWasChanged                 = 'Параметры были изменены. Сохранить?';
  constErrGetAdFromText1                = 'Ошибка по обработке объявления' + sLineBreak + sLineBreak + '№ строки из таблицы результатов: ';
  constErrGetAdFromText2                = 'Тлф: ';
  constErrGetAdFromText3                = 'Текст объявления: ';
  constErrGetAdFromText4                = 'текст ошибки: ' + sLineBreak;
  constErrGetAdFromText5                = 'OK - продолжить; ОТМЕНА - остановить';
  constProfileNotFound                  = 'Профиль с настройками для распознавания не найден:';
  constErrGetFullPath2                  = 'Процедура GetFullPath2 вызвала ошибку.';
  constInCorrectRexExp                  = 'Некорректный образец поиска:';
  constReplaceTxt                       = 'Заменить текст';
  constReplaceTxtRemark                 = 'в поле ПРИМЕЧАНИЕ';
  constEnterStreetName                  = 'Введите название улицы';
  constAddStreetToKvx                   = 'Добавить улицу в базу Квартал ПРО';
  constErrAddStreetToKvx                = 'Улица не была добавлена.';
  constDone                             = 'Готово';
  constDemoTxt                          = 'В демо версии можно импортировать в базу только 5 записей за один сеанс работы Сканфильтра';
  constDemoCaption                      = ' - ДЕМО ВЕРСИЯ - ТОЛЬКО 5 ЗАПИСЕЙ ЗА СЕАНС';
  constPriceNull                        = 'Внимание. Цена будет обнулена, т.е. равна 0 (нулю). Продолжить?';
  constErrPriceCalc                     = 'Ошибка пересчёта цены в строке №: ';
  constReplaceRooms                     = 'Заменить кол-во комат';
  constEnterRoomsCount                  = 'Введите новое кол-во комнат:';
  constEnterInt                         = 'Введите целое число';
  constErrAddADToBase                   = 'Ошибка при добавлении в таблицу ';
  constErrAddADToBase2                  = 'текст запроса: ' + sLineBreak;
  constPleaseWait                       = 'Пожалуйста ждите, идет обработка объявлений...';
  constMsgResetLayout                   = 'Будут сброшены настройки дизайнера форм, меню и таблиц. Продолжить?';


//AddTelsToExistingMed

  constSelMed                           = 'ВЫБЕРИТЕ ПОСРЕДНИКА:';
  constMsgErrorOnGetContentStyle        = 'Ошибка при перерисовке таблицы (OnGetContentStyle)';


//fmPreviewDB
  constInputNewPrice                    = 'Введите новую цену для ID';
  constInputNewPriceCap                 = 'Изменить цену объекта';
  constInputDelReason                   = 'Введите причину для ID';
  constInputDelReasonCap                = 'Причина удаления/восстановления';

implementation

end.
