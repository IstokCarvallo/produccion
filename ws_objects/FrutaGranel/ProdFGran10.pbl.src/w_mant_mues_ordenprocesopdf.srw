$PBExportHeader$w_mant_mues_ordenprocesopdf.srw
forward
global type w_mant_mues_ordenprocesopdf from w_mant_tabla
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_ordenprocesopdf
end type
type st_1 from statictext within w_mant_mues_ordenprocesopdf
end type
end forward

global type w_mant_mues_ordenprocesopdf from w_mant_tabla
integer width = 3401
string title = "MANTENEDOR DE ORDEN PROCESO PDF"
uo_selplanta uo_selplanta
st_1 st_1
end type
global w_mant_mues_ordenprocesopdf w_mant_mues_ordenprocesopdf

type variables
w_mant_deta_ordenprocesopdf	iw_mantencion
uo_orderprocesopdf		iuo_Pdf
end variables

forward prototypes
public function boolean wf_grabaimagenes ()
end prototypes

public function boolean wf_grabaimagenes ();Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

If dw_1.RowCount() < 1 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_1.RowCount()
		ls_Archivo	=	dw_1.Object.orpr_rutas[ll_Fila] + dw_1.Object.orpr_archiv[ll_Fila]
		
		iuo_Pdf.GrabaImagen(dw_1, ll_Fila, Sqlca, ls_Archivo)
	Next
End If

Return lb_Retorno
end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True
istr_mant.Argumento[1]	=	String(uo_SelPlanta.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "ORDEN PROCESO DE PDF"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelPlanta.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(gi_CodExport, uo_SelPlanta.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_ordenprocesopdf.create
int iCurrent
call super::create
this.uo_selplanta=create uo_selplanta
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selplanta
this.Control[iCurrent+2]=this.st_1
end on

on w_mant_mues_ordenprocesopdf.destroy
call super::destroy
destroy(this.uo_selplanta)
destroy(this.st_1)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False
istr_mant.Argumento[1]	=	String(uo_SelPlanta.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelPlanta.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	dw_1.SetRowFocusIndicator(Off!)
	
	iuo_Pdf	=	Create uo_orderprocesopdf

	buscar	= "Archivo:Sorpr_archiv,Tipo:Norpr_tipord,Numero:Norpr_numero,Prodcto:Sprod_nombre"
	ordenar	= "Archivo:orpr_archiv,Tipo:orpr_tipord,Numero:orpr_numero,Prodcto:prod_nombre"
End IF
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_mant.Argumento[1]	=	String(uo_SelPlanta.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_guardar;If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then Return

If wf_actualiza_db() Then
	wf_GrabaImagenes()
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ordenprocesopdf
integer y = 396
integer width = 2825
integer height = 1432
string dataobject = "dw_mues_ordenprocesopdf"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ordenprocesopdf
integer y = 84
integer width = 2825
integer height = 248
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ordenprocesopdf
integer x = 3054
integer y = 152
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ordenprocesopdf
integer x = 3040
integer y = 448
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ordenprocesopdf
integer x = 3054
integer y = 672
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ordenprocesopdf
integer x = 3054
integer y = 840
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ordenprocesopdf
integer x = 3054
integer y = 1008
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ordenprocesopdf
boolean visible = false
integer x = 3054
integer y = 1176
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ordenprocesopdf
integer x = 3054
integer y = 1552
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_ordenprocesopdf
integer x = 617
integer y = 172
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_1 from statictext within w_mant_mues_ordenprocesopdf
integer x = 192
integer y = 180
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
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

