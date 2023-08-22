$PBExportHeader$w_mant_mues_diasinpacking.srw
forward
global type w_mant_mues_diasinpacking from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_diasinpacking
end type
type st_2 from statictext within w_mant_mues_diasinpacking
end type
type em_fecha from editmask within w_mant_mues_diasinpacking
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_diasinpacking
end type
end forward

global type w_mant_mues_diasinpacking from w_mant_tabla
integer width = 2528
integer height = 2000
string title = "Días Sin Packing"
st_1 st_1
st_2 st_2
em_fecha em_fecha
uo_selplanta uo_selplanta
end type
global w_mant_mues_diasinpacking w_mant_mues_diasinpacking

type variables
uo_spro_ordenproceso		iuo_Proceso
end variables

forward prototypes
public function boolean duplicado (string as_valor, string as_columna)
end prototypes

public function boolean duplicado (string as_valor, string as_columna);Long        ll_fila
String      ls_fecini

ls_fecini 	= 	String(dw_1.Object.dspk_fecsin[il_fila], 'yyyy-mm-dd')
CHOOSE CASE as_columna
	CASE "dspk_fecsin"
		ls_fecini 	= 	as_valor
		
END CHOOSE

IF Len(ls_fecini) = 0 THEN
	Return False
END IF
    
ll_fila = dw_1.Find("String(dspk_fecsin, 'yyyy-mm-dd') 	= '" 	+ ls_fecini + "'", &
						  1,dw_1.RowCount())
						  
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Fecha ya ingresada anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF

end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

il_fila	=	dw_1.InsertRow(0)

pb_eliminar.Enabled	= TRUE
pb_grabar.Enabled		= TRUE

dw_1.SetRow(il_fila)
dw_1.SelectRow(0,False)

dw_1.SetColumn("dspk_fecsin")
dw_1.SetFocus()
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "INFORME DIAS SIN PACKING"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_diasinpacking"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, Date('01/' + em_fecha.Text))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta


DO
	ll_fila	= dw_1.Retrieve(uo_SelPlanta.Codigo, Date('01/' + em_Fecha.Text))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(0, False)
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

on w_mant_mues_diasinpacking.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_fecha=create em_fecha
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_fecha
this.Control[iCurrent+4]=this.uo_selplanta
end on

on w_mant_mues_diasinpacking.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.uo_selplanta)
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

istr_mant.borra	 = False
end event

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	iuo_Proceso	=	Create uo_spro_ordenproceso
	
	em_fecha.Text	=	String(Today(), 'MM/YYYY')
	istr_mant.argumento[2]	= 	'01/' + em_fecha.Text
End If
end event

event ue_modifica;//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer		li_filas
Date			ld_fechas

SetNull(ld_fechas)

FOR li_filas = 1 TO dw_1.RowCount()
	IF NOT IsDate( String(dw_1.Object.dspk_fecsin[li_filas], 'dd/mm/yyyy') ) THEN
		dw_1.DeleteRow(li_filas)
		li_filas --
	ELSE
		dw_1.Object.plde_codigo[li_filas] = uo_selPlanta.Codigo
	END IF
NEXT
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_diasinpacking
integer x = 69
integer y = 352
integer width = 1883
integer height = 1364
string dataobject = "dw_mues_diasinpacking"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nulo
Date		ld_fechai

ls_columna = dwo.name
SetNull(ls_nulo)

CHOOSE CASE ls_columna
	CASE "dspk_fecsin"
		IF Duplicado(data, ls_columna) THEN
			This.Object.dspk_fecsin[il_fila]	=	Date(ls_nulo)
			RETURN 1
		END IF
		
		IF Month(Date(data)) <> Month(Date(istr_mant.argumento[2])) THEN
			Messagebox("Error", "La Fecha debe estar dentro del mes de proceso", Exclamation!)
			This.Object.dspk_fecsin[il_fila]	=	Date(ls_nulo)
			RETURN 1
		END IF
		
		ld_fechai	=	Date(data)
		IF NOT iuo_proceso.procesoenfecha(Integer(istr_mant.Argumento[1]), -1, ld_fechai, True, sqlca) THEN
			This.Object.dspk_fecsin[il_fila]	=	Date(ls_nulo)
			MessageBox("Error", "El día ingresado tiene un proceso asignado", Information!)
			RETURN 1
		END IF
		
END CHOOSE	
end event

event dw_1::itemerror;call super::itemerror;This.SelectRow(0, False)
Return 1
end event

event dw_1::ue_seteafila;call super::ue_seteafila;This.SelectRow(0, False)
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;This.SelectRow(0, False)
end event

event dw_1::clicked;This.SelectRow(0, False)
end event

event dw_1::doubleclicked;call super::doubleclicked;This.SelectRow(0, False)
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_diasinpacking
integer x = 69
integer y = 60
integer width = 1883
integer height = 264
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_diasinpacking
integer x = 2144
integer y = 132
end type

event pb_lectura::clicked;call super::clicked;uo_SelPlanta.Bloquear(True)

end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_diasinpacking
integer x = 2144
integer y = 428
end type

event pb_nuevo::clicked;call super::clicked;dw_1.Reset()
uo_SelPlanta.Bloquear(False)
pb_insertar.enabled	=	False
pb_eliminar.enabled	=	False
pb_grabar.enabled	=	False
pb_imprimir.enabled	=	False
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_diasinpacking
integer x = 2144
integer y = 636
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_diasinpacking
integer x = 2144
integer y = 804
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_diasinpacking
integer x = 2144
integer y = 972
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_diasinpacking
integer x = 2144
integer y = 1140
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_diasinpacking
integer x = 2144
integer y = 1512
end type

type st_1 from statictext within w_mant_mues_diasinpacking
integer x = 329
integer y = 108
integer width = 215
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean border = true
long bordercolor = 12632256
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_diasinpacking
integer x = 329
integer y = 216
integer width = 379
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Mes Proceso"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_diasinpacking
integer x = 722
integer y = 200
integer width = 407
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
boolean dropdowncalendar = true
long calendartextcolor = 0
long calendartitletextcolor = 0
long calendartrailingtextcolor = 8421504
long calendarbackcolor = 12632256
long calendartitlebackcolor = 10789024
end type

event modified;istr_mant.argumento[2]	=	'01/' + This.Text
end event

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_diasinpacking
event destroy ( )
integer x = 722
integer y = 96
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

