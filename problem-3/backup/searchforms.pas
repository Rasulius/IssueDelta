unit searchforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LazUTF8, syncobjs, DateUtils;

type
  { TSearchForm }
  TSearchStatus = (ssFind, ssNotFind, ssTimout);
  TTextSearchThread = class;
  TShowStatusEvent = procedure(FStartTime, FEndTime: TTime; SearchStatus: TSearchStatus) of Object;

  TSearchForm = class(TForm)
    FindFirstButton: TButton;
    FindNextButton: TButton;
    SearchLine: TEdit;
    Statistic: TLabel;
    ReportBox: TListBox;
    DocumentMemo: TMemo;
    TopPanel: TPanel;
    procedure FindFirstButtonClick(Sender: TObject);
    procedure FindNextButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private
    FTextSearcher: TTextSearchThread;
    FText: TStringList;
    FFindIndex: Integer;
    FEvent: TEvent;
  public
    constructor Create(anOwner: TComponent; aText: TStringList); overload;
    destructor Destroy; override;

    procedure OnFindText(FStartTime, FEndTime: TTime; SearchStatus: TSearchStatus);
  end;

type
  { TTextSearchThread }

  TTextSearchThread = class (TThread)
  private
    FBufferSize: Integer;
    FFindText: String;
    FText: String;
    FTextLength: Integer;
    FOnShowStatus: TShowStatusEvent;
    FFindIndex: Integer;
    FEvent: TEvent;
    FWaitingTime: Integer;
    FSearchStatus: TSearchStatus;
    FPortionIndex: Integer;
    FFindPosition: Integer;
    { начало и конец запуска }
    FStartTime: TTime;
    FEndTime: TTime;
  protected
    procedure Execute; override;
    procedure ShowStatus();
  public
    constructor Create(CreateSuspendedThread: Boolean; anEvent: TEvent; WaitingTime: Integer = 100);
    procedure Init(const FindText: String; AllText: TStrings; Offset: Integer = 1);

    destructor Destroy; override;

    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;



procedure ShowSearchForm(Owner: TComponent; aText: TStringList);

var
  SearchForm: TSearchForm;
  FormCount: Integer;


implementation

{$R *.lfm}

procedure ShowSearchForm(Owner: TComponent;  aText: TStringList);
var
  SearchForm: TSearchForm;
begin
  SearchForm := TSearchForm.Create(Owner, aText);
  try
    SearchForm.Name := 'MyForm' + IntToStr(FormCount);
    Inc(FormCount);
    SearchForm.Show;
  except
    SearchForm.Free;
  end;
end;

{ TTextSearchThread }

constructor TTextSearchThread.Create(CreateSuspendedThread: Boolean; anEvent: TEvent; WaitingTime: Integer = 100);
begin
  inherited Create(CreateSuspendedThread);
  FTextLength := 0;
  FFindIndex := 1;
  FreeOnTerminate := True;
  FEvent := anEvent;
  FWaitingTime := WaitingTime;
  FBufferSize:= 50;
end;

procedure TTextSearchThread.Init(const FindText: String; AllText: TStrings; Offset: Integer);
begin
  FFindText := FindText;
  FFindIndex := Offset;
  FText := AllText.Text;
  FTextLength:= Utf8Length(FText);
end;

destructor TTextSearchThread.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

procedure TTextSearchThread.Execute;
var
  Portion: String;
  StopSearch: Boolean;
  StartIndex: Integer;
  BufferLength: Integer;
  PortionSize: Integer;
  DiffTime: Extended;
  TEMP: Integer;
begin
  FPortionIndex := 0;
  PortionSize := FBufferSize;
  FSearchStatus := ssNotFind;

  while not Terminated do begin
    FEvent.WaitFor(Infinite);
    FStartTime := Now;
    StopSearch := False;
    StartIndex := 0;
    PortionSize := FBufferSize;
    FPortionIndex := 0;
    FFindIndex := 1;
    FFindPosition := 0;
    while not StopSearch do begin
      if ((StartIndex + PortionSize) > FTextLength) then begin
        PortionSize := FTextLength - StartIndex;
        StopSearch := True;
      end;
      Sleep(1); // Имитация замедления

      Portion := Copy(FText, StartIndex, PortionSize*2);
      FFindIndex := Pos(FFindText, Portion, 1);
      FEndTime := Now;
      DiffTime:=  MillisecondOfTheDay(FStartTime-FEndTime);

       { timeout }
      if(DiffTime> FWaitingTime) then begin
        StopSearch := True;
        FSearchStatus := ssTimout;
        Synchronize(@ShowStatus);
      end;

      { find }

      if FFindIndex > 0 then begin
        FSearchStatus := ssFind;
        Synchronize(@ShowStatus);
        Break;
      end;
      StartIndex := StartIndex + 2*PortionSize;
      Inc(FPortionIndex);
    end;
    FEvent.ResetEvent;

    if FSearchStatus = ssNotFind then
      Synchronize(@ShowStatus);
  end;
end;

procedure TTextSearchThread.ShowStatus();
var
  Length: Integer;
begin
  if Assigned(FOnShowStatus) then begin
    FOnShowStatus(FStartTime, FEndTime, FSearchStatus);
  end;
end;

{ TSearchForm }

procedure TSearchForm.FindFirstButtonClick(Sender: TObject);
begin
  ReportBox.Items.Clear;
  FTextSearcher.Init(SearchLine.Text, DocumentMemo.Lines);
  FEvent.SetEvent;
  FTextSearcher.Start;
end;

procedure TSearchForm.FindNextButtonClick(Sender: TObject);
begin
  ReportBox.Items.Clear;
  FTextSearcher.Init(SearchLine.Text, DocumentMemo.Lines, FFindIndex + 2);
  FEvent.SetEvent;
  FTextSearcher.Start;
end;

procedure TSearchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTextSearcher.Terminate;
  CloseAction := caFree;
end;



constructor TSearchForm.Create(anOwner: TComponent; aText: TStringList);
begin
  inherited Create(anOwner);
  FText := TStringList.Create;

  if Length(aText.Text) >0 then begin
    FText.Assign(aText);
    DocumentMemo.Lines.Assign(FText);
  end;


  FEvent := TEvent.Create(nil,False, False, 'SearchThreadEvent'+IntToStr(FormCount));
  FTextSearcher := TTextSearchThread.Create(True, FEvent);
  FTextSearcher.OnShowStatus:= @OnFindText;
  FFindIndex := 1;
end;

destructor TSearchForm.Destroy;
begin
  FTextSearcher.Terminate;
  FEvent.ResetEvent;
  FEvent.Free;
  FText.Free;
  inherited Destroy;
end;

procedure TSearchForm.OnFindText(FStartTime, FEndTime: TTime; SearchStatus: TSearchStatus);
Const
  ColumnCaption = 'Время старта Время завершения Успех Время выполнения';
  TextCaption   = '%s           %s                     %s    %s';
begin
  case  SearchStatus of
     ssFind: begin
       ReportBox.Items.Add(ColumnCaption);
       ReportBox.Items.Add(Format(TextCaption,
        [FormatDateTime('hh:nn:ss:zzz', FStartTime),
        FormatDateTime('hh:nn:ss:zzz', FEndTime) ,
        'Да',
        FormatDateTime('hh:nn:ss:zzz',FEndTime - FStartTime)
        ]));
     end;
     ssNotFind: begin
       ReportBox.Items.Add(ColumnCaption);
       ReportBox.Items.Add(Format(TextCaption,
        [FormatDateTime('hh:nn:ss:zzz', FStartTime),
        FormatDateTime('hh:nn:ss:zzz', FEndTime) ,
        'Да',
        FormatDateTime('hh:nn:ss:zzz',FEndTime - FStartTime)
        ]));
     end;
     ssTimout: begin
       ReportBox.Items.Add(ColumnCaption);
       ReportBox.Items.Add(Format(TextCaption,
        [FormatDateTime('hh:nn:ss:zzz', FStartTime),
        FormatDateTime('hh:nn:ss:zzz', FEndTime) ,
        'Да',
        FormatDateTime('hh:nn:ss:zzz',FEndTime - FStartTime)
        ]));
     end;
  end
end;

end.

