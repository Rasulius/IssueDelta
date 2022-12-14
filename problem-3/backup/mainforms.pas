unit mainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateSearchFormButton: TButton;
    LoadDocumentButton: TButton;
    SearchFormsList: TListBox;
    OpenDialog: TOpenDialog;

    procedure CreateSearchFormButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadDocumentButtonClick(Sender: TObject);
  private
    FText: TStringList;
    procedure PrintAllSearchForms(const ClosedForm: String = '');
  protected
    procedure OnCloseSearchForm(const ClosedFormName: String; Sender: TObject; var CloseAction: TCloseAction);

  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;

  end;

var
  MainFormA: TMainForm;

implementation

uses searchforms;

{$R *.lfm}

{ TMainForm }


constructor TMainForm.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FText := TStringList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FText);
  inherited Destroy;
end;

procedure TMainForm.LoadDocumentButtonClick(Sender: TObject);
begin
 if OpenDialog.Execute then begin
   FText.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.PrintAllSearchForms(const ClosedForm: String = '');
var
   i: Integer;
begin
  SearchFormsList.Clear;

  for i := 0 to Application.ComponentCount - 1 do begin
    if (Application.Components[i] is TSearchForm)
       and ((Application.Components[i] as TSearchForm).Name <> ClosedForm)  then begin
      SearchFormsList.Items.Add((Application.Components[i] as TSearchForm).Name);
    end;
  end;

end;

procedure TMainForm.OnCloseSearchForm(const ClosedFormName: String; Sender: TObject;
  var CloseAction: TCloseAction);
begin
 // Передаем в качестве параметра форма которая будет закрыта
  PrintAllSearchForms(ClosedFormName);
end;


procedure TMainForm.CreateSearchFormButtonClick(Sender: TObject);
begin
  ShowSearchForm(Owner, FText, @OnCloseSearchForm);
  PrintAllSearchForms;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

end.

