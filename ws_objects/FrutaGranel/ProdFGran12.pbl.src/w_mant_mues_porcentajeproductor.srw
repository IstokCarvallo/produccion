$PBExportHeader$w_mant_mues_porcentajeproductor.srw
forward
global type w_mant_mues_porcentajeproductor from w_mant_directo
end type
type uo_seleprod from uo_seleccion_productor within w_mant_mues_porcentajeproductor
end type
type st_1 from statictext within w_mant_mues_porcentajeproductor
end type
end forward

global type w_mant_mues_porcentajeproductor from w_mant_directo
integer width = 2039
string title = "MANTENCION DE PORCENTAJES POR PRODUCTOR"
uo_seleprod uo_seleprod
st_1 st_1
end type
global w_mant_mues_porcentajeproductor w_mant_mues_porcentajeproductor

type variables
uo_especie	iuo_especie
end variables

forward prototypes
public function boolean duplicado (string as_especie)
end prototypes

public function boolean duplicado (string as_especie);Integer	li_fila

li_fila	=	dw_1.Find("espe_codigo = " + as_especie, 1, dw_1.RowCount())

IF li_fila > 0 THEN
	MessageBox("Error de Validación", "La especie (" + as_especie + ") ya fue ingresada")
	Return True
	
ELSE
	Return False
	
END IF
end function

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_seleprod.codigo) THEN lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_seleprod.Seleccion(False, False)
	uo_seleprod.Filtra(-1)
	
	iuo_especie	=	Create uo_especie
	
	buscar	=	"Código Especie:Nespe_codigo,Porcentaje Descuento:Npopr_porcen"
	ordenar	=	"Código Especie:espe_codigo,Porcentaje Descuento:popr_porcen"
End IF
end event

on w_mant_mues_porcentajeproductor.create
int iCurrent
call super::create
this.uo_seleprod=create uo_seleprod
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_seleprod
this.Control[iCurrent+2]=this.st_1
end on

on w_mant_mues_porcentajeproductor.destroy
call super::destroy
destroy(this.uo_seleprod)
destroy(this.st_1)
end on

event ue_recuperadatos;Long	ll_fila, respuesta

DO
	dw_1.SetTransObject(SQLCA)
	ll_fila	= dw_1.Retrieve(uo_seleprod.codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= 	True
		pb_eliminar.Enabled	= 	True
		pb_grabar.Enabled		= 	True
		pb_imprimir.Enabled	= 	True
		il_fila					= 	1
		uo_seleprod.Enabled	=	False
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
		uo_seleprod.Enabled	=	False
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.popr_porcen[il_fila]) OR dw_1.Object.popr_porcen[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPorcentaje Descuento"
	ls_colu[li_cont]	= "popr_porcen"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetColumn("espe_codigo")
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;FOR il_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! OR &
		dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			Exit
		END IF
	END IF
NEXT

IF Message.DoubleParm = -1 THEN
	Message.DoubleParm = -1
	Return
END IF

FOR il_fila = 1 TO dw_1.RowCount() 
	dw_1.Object.prod_codigo[il_fila]	=	uo_seleprod.Codigo
	dw_1.SetRow(il_fila)
	
NEXT
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO PORCENTAJE DE DESCUENTO POR PRODUCTOR"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_porcentajeproductor"
vinf.dw_1.SetTransObject(sqlca)

IF MessageBox("Emisión de Informe", "¿Desea imprimir todos los productores?", Question!, YesNo!) = 1 THEN
	fila = vinf.dw_1.Retrieve(-1)
ELSE
	fila = vinf.dw_1.Retrieve(uo_seleprod.Codigo)
END IF

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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_porcentajeproductor
integer y = 16
integer width = 1408
integer height = 272
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 400
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;uo_seleprod.Enabled	=	True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 104
integer taborder = 30
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 760
integer taborder = 60
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 580
integer taborder = 50
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 1504
integer taborder = 90
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 1120
integer taborder = 80
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_porcentajeproductor
integer x = 1664
integer y = 940
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_porcentajeproductor
integer y = 308
integer width = 1408
integer height = 1500
integer taborder = 20
string dataobject = "dw_mues_porcentajeproductor"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "espe_codigo"
		IF Duplicado(data) OR Not iuo_especie.Existe(Integer(data), True, SQLCa) THEN
			This.Object.espe_codigo[row]	=	li_null
			Return 1
		END IF
		
	CASE "popr_porcen"
		IF Integer(data) < 0 THEN
			MessageBox("Error", "El porcentaje debe ser un decimal superior a 0")
			This.Object.popr_porcen[row] = li_null
			Return 1
		END IF
		
END CHOOSE
end event

type uo_seleprod from uo_seleccion_productor within w_mant_mues_porcentajeproductor
integer x = 530
integer y = 112
integer height = 80
integer taborder = 10
boolean bringtotop = true
end type

on uo_seleprod.destroy
call uo_seleccion_productor::destroy
end on

type st_1 from statictext within w_mant_mues_porcentajeproductor
integer x = 174
integer y = 124
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

