$PBExportHeader$w_mant_mues_corrector.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_corrector from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_corrector
end type
type em_pallet from editmask within w_mant_mues_corrector
end type
end forward

global type w_mant_mues_corrector from w_mant_directo
integer width = 4123
event ue_validapassword ( )
st_1 st_1
em_pallet em_pallet
end type
global w_mant_mues_corrector w_mant_mues_corrector

type variables
Integer				ii_tipo

uo_ProdPredio 		iuo_Predio
uo_ProdCuarteles	iuo_Cuartel
uo_Mail				iuo_Mail
end variables

event ue_validapassword();Str_mant		lstr_mant

lstr_mant.Argumento[1]	=	"Correcion de Tablas"
lstr_mant.Argumento[2]	=	'ADM' + gs_Password + 'CORRIGE'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(Long(em_pallet.Text))

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_corrector.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_pallet=create em_pallet
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_pallet
end on

on w_mant_mues_corrector.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_pallet)
end on

event open;call super::open;ii_Tipo = Integer(Message.StringParm)

iuo_Predio	=	Create uo_ProdPredio
iuo_Cuartel	=	Create uo_ProdCuarteles
iuo_Mail		=	Create uo_Mail

IF ii_Tipo = 1 Then
	This.Title = 'Correcion en Pallet Encab'
	dw_1.DataObject = 'dw_cons_palletencab'
ElseIf ii_Tipo = 2 Then
	This.Title = 'Correcion en Pallet Fruta'
	dw_1.DataObject = 'dw_cons_palletfruta'	
Else
	This.Title = 'Correcion en Pallet Fruta Histo'
	dw_1.DataObject = 'dw_cons_palletfrutahisto'
End If

dw_1.SettransObject(Sqlca)

PostEvent("ue_validapassword")
end event

event ue_guardar;String 	ls_Correo, ls_Asunto, ls_Texto, ls_Error
Integer	li_Correo 

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

ls_Correo 	= '<gabriel.ponce@rioblanco.net>'
//ls_Correo 	= '<istok.carvallo@rioblanco.net>'
ls_asunto	= 'Modificacion de Folio : ' + em_pallet.Text
ls_texto  		= 'Se modifica Folio : (' + em_pallet.Text + ')~n~n' + This.Title + '~n~nUsuario:' + gstr_Us.Nombre + &
					'~n~nComputador:' + gstr_us.Computador + '~n~nBase de Datos: ' + gstr_apl.Odbc

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	iuo_mail.Of_Send({ls_correo},ls_asunto,ls_texto,0)							
	
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_corrector
integer x = 82
integer y = 36
integer width = 3456
integer height = 244
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_corrector
integer x = 3749
integer y = 424
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
	//	IF gi_codplanta <> 2 AND gi_codplanta <> 3 AND gi_codplanta <> 4 THEN
			istr_mant.Solo_Consulta = True
	//	END IF	
	END IF	
END IF	

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_corrector
integer x = 3749
integer y = 152
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_corrector
boolean visible = false
integer x = 3749
integer y = 808
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_corrector
boolean visible = false
integer x = 3749
integer y = 628
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_corrector
integer x = 3749
integer y = 1552
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_corrector
boolean visible = false
integer x = 3749
integer y = 1168
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_corrector
integer x = 3749
integer y = 988
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_corrector
integer x = 14
integer y = 324
integer width = 3456
integer height = 1532
integer taborder = 70
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Null
Long	ll_Valor

SetNull(ls_Null)

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "pafr_huert1", "pafr_huert4"
		ll_Valor		=	This.GetItemNumber(Row, ls_Columna)
		If Not iuo_Predio.Existe(Long(Data), This.Object.prod_codigo[Row], True, Sqlca) Then
			This.SetItem(Row, ls_Columna, ll_Valor)
			Return 1
		End If
		
	Case "pafr_cuart1"
		ll_Valor		=	This.GetItemNumber(Row, ls_Columna)
		If Not iuo_Cuartel.Existe(This.Object.prod_codigo[Row], This.Object.pafr_huert1[Row], Long(Data), This.Object.espe_codigo[Row], This.Object.vari_codigo[Row], True, Sqlca) Then
			This.SetItem(Row, ls_Columna, ll_Valor)
			Return 1
		End If
		
	Case "pafr_cuart4"
		ll_Valor		=	This.GetItemNumber(Row, ls_Columna)
		If Not iuo_Cuartel.Existe(This.Object.prod_codigo[Row], This.Object.pafr_huert4[Row], Long(Data), This.Object.espe_codigo[Row], This.Object.vari_codigo[Row], True, Sqlca) Then
			This.SetItem(Row, ls_Columna, ll_Valor)
			Return 1
		End If
		
End Choose
end event

type st_1 from statictext within w_mant_mues_corrector
integer x = 695
integer y = 120
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_pallet from editmask within w_mant_mues_corrector
integer x = 1088
integer y = 92
integer width = 658
integer height = 112
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

