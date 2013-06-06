unit NameEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, LazIDEIntf, PropEdits;

type
  TfrmNameEdit = class(TForm)
    btnDelete : TBitBtn;
    btnEdit : TBitBtn;
    btnCancel : TBitBtn;
    btnOK : TBitBtn;
    btnSave : TBitBtn;
    btnCancelPrefix : TBitBtn;
    cbxClass : TComboBox;
    editPrefix : TEdit;
    editName : TEdit;
    rgScope : TRadioGroup;
    procedure btnEditClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
  private
    fldSysPrefs : TStrings;
    fldUsrPrefs : TStrings;
    fldPrjPrefs : TStrings;
    class function SysPrefsFile : String;
    class function UsrPrefsFile : String;
    class function PrjPrefsFile : String;
    procedure editNameMode;
    procedure editPrefixMode;
    procedure fillClasses;
    procedure loadPrefixes;
  public
    Edited : TComponent;
    ResultName : String;
    ResultCaption : String;
    OldCaption : String;
    procedure ParseName;
    procedure FindPrefix;
    class function Execute (cmp : TComponent) : Boolean;
  end;

type
  TNameEdit = class(TComponentNamePropertyEditor)
    function GetAttributes : TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{$R *.lfm}

procedure Register;
 begin
  RegisterPropertyEditor(TypeInfo(AnsiString), TComponent, 'Name', TNameEdit);
 end;

const
  PrefsFileName = 'nameedit.lst';

{ TfrmNameEdit }

procedure TfrmNameEdit.btnEditClick(Sender : TObject);
 begin

 end;

procedure TfrmNameEdit.FormCreate(Sender : TObject);
 begin
  fldSysPrefs := TStringList.Create;
  fldUsrPrefs := TStringList.Create;
  fldPrjPrefs := TStringList.Create;
  loadPrefixes;
 end;

class function TfrmNameEdit.SysPrefsFile : String;
 begin
  result := LazarusIDE.GetSecondaryConfigPath + '/' + PrefsFileName;
 end;

class function TfrmNameEdit.UsrPrefsFile : String;
 begin
  result := LazarusIDE.GetPrimaryConfigPath + '/' + PrefsFileName;
 end;

class function TfrmNameEdit.PrjPrefsFile : String;
 begin
  result := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename) + '.' + PrefsFileName;
 end;

procedure TfrmNameEdit.editNameMode;
 begin
  editPrefix.Enabled := false;
  editName.Visible := true;
  btnEdit.Visible := true;
  btnCancel.Visible := true;
  btnOK.Visible := true;
  btnSave.Visible := false;
  btnCancelPrefix.Visible := false;
  cbxClass.Visible := false;
  btnDelete.Visible := false;
  rgScope.Visible := false;
 end;

procedure TfrmNameEdit.editPrefixMode;
 begin
  editPrefix.Enabled := true;
  editName.Visible := false;
  btnEdit.Visible := false;
  btnCancel.Visible := false;
  btnOK.Visible := false;
  btnSave.Visible := true;
  btnCancelPrefix.Visible := true;
  cbxClass.Visible := true;
  cbxClass.ItemIndex := 0;
  btnDelete.Visible := true;
  rgScope.Visible := true;
 end;

procedure TfrmNameEdit.fillClasses;
 var
   cls : TClass;
 begin
  cls := Edited.ClassType;
  cbxClass.Clear;
  if not ((Edited is TForm) or (Edited is TDataModule) or (Edited is TFrame))
     then cbxClass.AddItem(cls.ClassName, nil);
  repeat
    cls := cls.ClassParent;
    cbxClass.AddItem(cls.ClassName, nil);
  until (cls = TComponent) or (cls = TObject);
 end;

procedure TfrmNameEdit.loadPrefixes;
 begin
  fldSysPrefs.Clear;
  if FileExists(SysPrefsFile)
     then fldSysPrefs.LoadFromFile(SysPrefsFile);
  fldUsrPrefs.Clear;
  if FileExists(UsrPrefsFile)
     then fldUsrPrefs.LoadFromFile(UsrPrefsFile);
  fldPrjPrefs.Clear;
  if FileExists(PrjPrefsFile)
     then fldPrjPrefs.LoadFromFile(PrjPrefsFile);
 end;

procedure TfrmNameEdit.ParseName;
 var
   i : Integer;
 begin
  for i := 1 to Length(Edited.Name) do
      if not (Edited.Name[i] in ['a'..'z', '0'..'9', '_'])
         then begin
               OldCaption := Copy(Edited.Name, i, Length(Edited.Name) - i + 1);
               editName.Text := OldCaption;
               Exit;
              end;
  OldCaption := Edited.Name;
  editName.Text := OldCaption;
 end;

procedure TfrmNameEdit.FindPrefix;
 var
   cls : TClass;
   pfx : String;
 begin
  cls := Edited.ClassType;
  repeat
    pfx := fldPrjPrefs.Values[cls.ClassName];
    if pfx <> ''
       then begin
             editPrefix.Text := pfx;
             Exit;
            end;
    pfx := fldUsrPrefs.Values[cls.ClassName];
    if pfx <> ''
       then begin
             editPrefix.Text := pfx;
             Exit;
            end;
    pfx := fldSysPrefs.Values[cls.ClassName];
    if pfx <> ''
       then begin
             editPrefix.Text := pfx;
             Exit;
            end;
    cls := cls.ClassParent;
  until (cls = TComponent) or (cls = TObject);
  editPrefix.Text := 'cmp';
 end;

var
  frmNameEdit : TfrmNameEdit = nil;

class function TfrmNameEdit.Execute(cmp : TComponent) : Boolean;
 begin
  if frmNameEdit = nil
     then frmNameEdit := TfrmNameEdit.Create(nil);
  frmNameEdit.Edited := cmp;
  frmNameEdit.ParseName;
  frmNameEdit.FindPrefix;
  if frmNameEdit.ShowModal = mrOK
     then begin
           cmp.Name := frmNameEdit.ResultName;
           if (cmp is TControl) and (TControl(cmp).Caption = frmNameEdit.OldCaption)
              then TControl(cmp).Caption := frmNameEdit.ResultCaption;
           result := true;
          end
     else result := false;
 end;

{ TNameEdit }

function TNameEdit.GetAttributes : TPropertyAttributes;
 begin
  result := [paDialog];
 end;

procedure TNameEdit.Edit;
 begin
  TfrmNameEdit.Execute(TComponent(GetComponent(0)));
 end;

end.

