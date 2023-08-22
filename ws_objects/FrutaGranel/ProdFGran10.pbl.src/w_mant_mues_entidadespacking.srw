$PBExportHeader$w_mant_mues_entidadespacking.srw
forward
global type w_mant_mues_entidadespacking from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_entidadespacking
end type
type st_2 from statictext within w_mant_mues_entidadespacking
end type
type dw_cliente from datawindow within w_mant_mues_entidadespacking
end type
type st_3 from statictext within w_mant_mues_entidadespacking
end type
type em_turno from editmask within w_mant_mues_entidadespacking
end type
type dw_contratista from datawindow within w_mant_mues_entidadespacking
end type
type dw_planta from datawindow within w_mant_mues_entidadespacking
end type
end forward

global type w_mant_mues_entidadespacking from w_mant_directo
integer width = 3922
integer height = 1832
string title = "CODIFICACIÓN PERSONAL PACKING"
boolean resizable = false
windowtype windowtype = child!
st_1 st_1
st_2 st_2
dw_cliente dw_cliente
st_3 st_3
em_turno em_turno
dw_contratista dw_contratista
dw_planta dw_planta
end type
global w_mant_mues_entidadespacking w_mant_mues_entidadespacking

type variables
DataWindowChild		idwc_cliente, idwc_contratista, idwc_planta, idwc_lineas
str_mant					istr_mant2

uo_plantadesp			iuo_plantadesp
uo_faenas				iuo_faenas
uo_contratista			iuo_contratista
String						is_rut
end variables

forward prototypes
public function decimal existeentidad (string as_rut)
public function boolean duplicado (string as_valor)
public function boolean existesalida (integer ai_linea)
end prototypes

public function decimal existeentidad (string as_rut);String 	ls_rut, ls_nombre, ls_apepat, ls_apemat
Long		ll_nrotar

select enpa_rutper
into :ls_rut
from dbo.spro_entidadespacking
where enpa_rutper = :as_rut;

IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de Tabla la spro_entidadespacking" )
		RETURN -1
		
ELSEIF sqlca.SQLCode = 100 THEN
	
	select 	pers_rutemp, pers_nombre, pers_apepat, pers_apemat, pers_nrotar
	into 		:ls_rut, 	 :ls_nombre,  :ls_apepat,  :ls_apemat,  :ll_nrotar
	from 		dbo.remupersonal
	where	pers_rutemp = :as_rut;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla remupersonal" )
		RETURN -1
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "El Rut ingresado no pertenece a un trabajador de Rio Blanco." + &
								   "~r~nIngrese o seleccione otro", StopSign!)
		Return -1
	END IF
	
	dw_1.Object.enpa_nombre[il_fila]	=	ls_apepat + ' ' + ls_apemat + ' ' + ls_nombre
	dw_1.Object.enpa_codper[il_fila]	=	ll_nrotar
	RETURN 1
	
END IF

RETURN 0
end function

public function boolean duplicado (string as_valor);Long		ll_fila
String	ls_codigo, ls_rut

ls_rut			=	dw_1.Object.enpa_rutper[dw_1.GetRow()]
ls_codigo		=	as_valor

ll_fila	= dw_1.Find("enpa_rutper	='" + ls_rut	+	"' AND " + &
							"faen_codigo	= " + ls_codigo, 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Advertencia", "La Faena " + ls_codigo + " ya ha sido ingresada para esta persona.~r~n" + &
									  "Ingrese o seleccione otro.", Exclamation!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existesalida (integer ai_linea);String 	ls_descripcion
Boolean	respuesta = TRUE
Integer	li_planta

li_planta =	dw_planta.Object.plde_codigo[1]

SELECT lisa_descri INTO :ls_descripcion
	FROM dbo.spro_salidaspacking
	WHERE lisa_codigo 	= 	:ai_linea
		and  plde_codigo 	= 	:li_planta;
 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_correlcompequipo.")
	Respuesta = False
ELSEIF sqlca.SQLCode = 100 THEN
	Respuesta = False
	MessageBox("Error","El codigo de salida seleccionada no existe.")
ELSE
	dw_1.Object.lisa_descri[dw_1.GetRow()]	=	ls_descripcion
END IF


Return Respuesta
end function

on w_mant_mues_entidadespacking.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.dw_cliente=create dw_cliente
this.st_3=create st_3
this.em_turno=create em_turno
this.dw_contratista=create dw_contratista
this.dw_planta=create dw_planta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_cliente
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_turno
this.Control[iCurrent+6]=this.dw_contratista
this.Control[iCurrent+7]=this.dw_planta
end on

on w_mant_mues_entidadespacking.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_cliente)
destroy(this.st_3)
destroy(this.em_turno)
destroy(this.dw_contratista)
destroy(this.dw_planta)
end on

event open;call super::open;dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1] = gstr_ParamPlanta.CodigoPlanta

dw_cliente.GetChild("enpa_rutper", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)

dw_1.GetChild("enpa_lineas", idwc_lineas)
idwc_lineas.SetTransObject(SQLCA)

istr_mant.Argumento[1]	=	""
istr_mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.Argumento[3]	=	"1"

iuo_plantadesp				=	Create uo_plantadesp
iuo_faenas					=	Create uo_faenas
iuo_contratista			=	Create uo_contratista

ordenar						= " Rut:enpa_rutper, Nombre:enpa_nombre, Faena:faen_codigo, Tarjeta:enpa_codper"
end event

event ue_recuperadatos;Integer respuesta, li_tipo

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

li_tipo = 0 //ExisteEntidad(is_rut)

IF li_tipo = 0 THEN
	idwc_lineas.Retrieve(Integer(istr_mant.Argumento[2]))
	
	DO			
		IF dw_1.Retrieve(Integer(istr_mant.Argumento[2])) = -1 THEN
			respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
		ELSE
			pb_grabar.Enabled	= True
			pb_imprimir.Enabled	= True
			pb_insertar.Enabled	= True
			pb_eliminar.Enabled	= True
			
			dw_1.SetColumn("enpa_rutper")
			dw_1.SetFocus()
		END IF
	LOOP WHILE respuesta = 1
	
	IF respuesta = 2 THEN Close(This)
	
ELSEIF li_tipo = 1 THEN 
	THIS.TriggerEvent("ue_nuevo")
ELSE
	Return
END IF
end event

event ue_nuevo;call super::ue_nuevo;
dw_1.Object.enpa_rutper[il_fila]	=	is_rut
dw_1.Object.plde_codigo[il_fila]	=	Integer(istr_mant.Argumento[2])

dw_1.SetFocus()
dw_1.SetColumn("enpa_rutper")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "LISTADO DE CODIGOS PERSONALES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_entidadespacking"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_entidadespacking
integer x = 82
integer y = 64
integer width = 3419
integer height = 264
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_entidadespacking
integer x = 3630
integer y = 424
end type

event pb_nuevo::clicked;call super::clicked;//dw_planta.Enabled 	=	True
//dw_cliente.Enabled	=	True
//dw_planta.Object.plde_codigo.background.color	=	rgb(255, 255, 255)
//dw_cliente.Object.clie_codigo.background.color	=	rgb(255, 255, 255)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_entidadespacking
integer x = 3630
integer y = 128
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_entidadespacking
integer x = 3630
integer y = 784
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_entidadespacking
integer x = 3630
integer y = 604
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_entidadespacking
integer x = 3630
integer y = 1500
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_entidadespacking
integer x = 3630
integer y = 1144
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_entidadespacking
integer x = 3630
integer y = 964
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_entidadespacking
integer x = 82
integer y = 344
integer width = 3419
integer height = 1356
boolean titlebar = true
string title = "Codigos Para Entidad"
string dataobject = "dw_mues_spro_entidadespacking"
end type

event dw_1::itemchanged;call super::itemchanged;Integer 				li_null, li_validarut
String				ls_columna, ls_rut
uo_lineapacking	luo_linea

SetNull(li_null)
ls_columna = dwo.name

CHOOSE CASE ls_columna
	
	CASE "line_codigo"
		luo_linea	=	Create uo_lineapacking
		
		IF NOT luo_linea.Existe(dw_planta.Object.plde_codigo[1], Integer(data), TRUE, sqlca) THEN
			This.SetItem(row, ls_columna, li_null)
			Return 1
		END IF
		
	CASE "enpa_salida"
		IF NOT ExisteSalida(integer(data)) THEN
			dw_1.SetItem(il_fila, "enpa_salida", Integer(li_null))
			dw_1.SetItem(il_fila, "lisa_descri", String(li_null))
			RETURN 1	
		END IF
		
	CASE "enpa_rutper"
		is_rut	=	F_verrut(data, True)
		IF is_rut	=	""	THEN
			This.SetItem(il_fila,ls_columna,String(li_null))
			Return 1
		END IF
		li_validarut = ExisteEntidad(data) 
		IF li_validarut = 0 THEN
			MessageBox("Error", "El rut que ha ingresado existe en la base de datos.~r~n" + &
										"Ingrese o seleccione otro.", StopSign!)
			This.Object.enpa_rutper[il_fila]	=	String(li_null)
			This.Object.enpa_nombre[il_fila]	=	String(li_null)
			Return 1
		ELSEIF li_validarut = -1 THEN
			This.Object.enpa_rutper[il_fila]	=	String(li_null)
			This.Object.enpa_nombre[il_fila]	=	String(li_null)
			Return 1
		END IF
		
	CASE "faen_codigo"
		IF duplicado(data) THEN
			dw_1.object.faen_codigo[il_fila]	=	li_null
			Return 1
		END IF
		IF NOT iuo_faenas.existe(Integer(data), True, SQLCA) THEN
			dw_1.object.faen_codigo[il_fila]	=	li_null
			Return 1
		END IF
		
	CASE "cont_codigo"
		IF NOT iuo_contratista.existe(Integer(data), True, SQLCA) THEN
			dw_1.object.cont_codigo[il_fila]	=	li_null
			Return 1
		END IF
			
END CHOOSE
end event

event dw_1::clicked;//
end event

event dw_1::buttonclicked;call super::buttonclicked;Integer 			li_null, li_validarut
String			ls_columna
Str_busqueda	lstr_busq

SetNull(li_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "b_busca"
		lstr_busq.argum[1]	=	''
		
		OpenWithParm(w_busc_remupersonal, lstr_busq)
		lstr_busq				=	Message.PowerObjectParm
		
		IF lstr_busq.argum[1] <> '' THEN
			li_validarut = ExisteEntidad(lstr_busq.argum[1]) 
			IF li_validarut = 0 THEN
				MessageBox("Error", "El rut que ha ingresado existe en la base de datos.~r~n" + &
											"Ingrese o seleccione otro.", StopSign!)
				This.Object.enpa_rutper[il_fila]	=	String(li_null)
				This.Object.enpa_nombre[il_fila]	=	String(li_null)
				Return 1
			ELSEIF li_validarut = -1 THEN
				This.Object.enpa_rutper[il_fila]	=	String(li_null)
				This.Object.enpa_nombre[il_fila]	=	String(li_null)
				Return 1
			ELSE
				This.Object.enpa_rutper[il_fila]	=	lstr_busq.argum[1]
			END IF
		END IF
		
	CASE "b_salida"
		lstr_busq.argum[1]	=	String(dw_planta.Object.plde_codigo[1])
		lstr_busq.argum[2]	=	String(This.Object.enpa_lineas[Row])
		
		OpenWithParm(w_busc_spro_salidaspacking, lstr_busq)
		lstr_busq				=	Message.PowerObjectParm
		
		IF lstr_busq.argum[1] <> '0' THEN
			This.Object.enpa_salida[Row]	=	Integer(lstr_busq.argum[3])
			This.Object.lisa_descri[Row]	=	lstr_busq.argum[4]
		END IF
		
		
END CHOOSE

end event

type st_1 from statictext within w_mant_mues_entidadespacking
boolean visible = false
integer x = 105
integer y = 240
integer width = 325
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
string text = "Entidad"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_entidadespacking
integer x = 1047
integer y = 164
integer width = 325
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
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_mant_mues_entidadespacking
boolean visible = false
integer x = 443
integer y = 224
integer width = 942
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_entidadespacking"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "enpa_rutper"
		is_rut	=	F_verrut(data, True)
		IF is_rut	=	""	THEN
			This.SetItem(il_fila,ls_columna,String(ll_null))
			Return 1
		END IF
			
END CHOOSE
end event

type st_3 from statictext within w_mant_mues_entidadespacking
boolean visible = false
integer x = 1993
integer y = 1792
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Turno"
boolean focusrectangle = false
end type

type em_turno from editmask within w_mant_mues_entidadespacking
boolean visible = false
integer x = 2341
integer y = 1776
integer width = 219
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#"
double increment = 1
string minmax = "1~~9"
end type

event modified;	istr_mant.Argumento[3]	=	This.Text
end event

type dw_contratista from datawindow within w_mant_mues_entidadespacking
boolean visible = false
integer x = 2331
integer y = 128
integer width = 1161
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "cont_codigo"
		istr_mant.Argumento[2]	=	data
		
END CHOOSE
end event

type dw_planta from datawindow within w_mant_mues_entidadespacking
integer x = 1390
integer y = 148
integer width = 942
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "plde_codigo"
		IF iuo_plantadesp.Existe(Integer(data), true, sqlca) THEN
				
			istr_mant.Argumento[2]	=	data
		ELSE
			Return -1
		END IF
END CHOOSE
end event

event itemerror;Return -1
end event

