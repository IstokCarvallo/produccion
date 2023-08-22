$PBExportHeader$w_mant_faenasembalajes.srw
forward
global type w_mant_faenasembalajes from w_mant_directo
end type
end forward

global type w_mant_faenasembalajes from w_mant_directo
integer width = 4690
string title = "FAENAS REALES POR EMBALAJE"
end type
global w_mant_faenasembalajes w_mant_faenasembalajes

type variables

uo_faenas			iuo_faena
end variables

forward prototypes
public function boolean duplicado (string as_valor, string as_columna)
end prototypes

public function boolean duplicado (string as_valor, string as_columna);Long        ll_fila
String      ls_faena, ls_embalaje, ls_alter, ls_valor, ls_especie

ls_faena 	= 	String(dw_1.Object.faen_codigo[il_fila])
ls_embalaje	=	dw_1.Object.emba_codigo[il_fila]
ls_alter 	= 	String(dw_1.Object.faen_codalt[il_fila])
ls_especie	=	String(dw_1.Object.espe_codigo[il_fila])

CHOOSE CASE as_columna
	CASE "faen_codigo"
		ls_faena 	= 	as_valor
		
	CASE "emba_codigo"
		ls_embalaje	= 	as_valor
		
	CASE "faen_codalt"
		ls_alter 	= 	as_valor
		
	CASE "espe_codigo"
		ls_especie 	= 	as_valor
		
END CHOOSE

IF ls_faena = ls_alter THEN
	MessageBox("Error","Faena Real no puede ser igual a la Faena Original",Information!, OK!)
	RETURN TRUE
END IF

IF NOT IsNull(ls_faena) THEN
	ll_fila = dw_1.Find("faen_codalt	= " 	+ ls_faena, 1,dw_1.RowCount())
END IF

IF NOT IsNull(ls_faena) AND NOT IsNull(ls_especie) THEN
	ll_fila = dw_1.Find("faen_codalt	= " 	+ ls_faena 		+	" and " + &
							  "espe_codigo	= " 	+ ls_especie, 1,dw_1.RowCount())
END IF

IF NOT IsNull(ls_faena) AND NOT IsNull(ls_embalaje) THEN
	ll_fila = ll_fila + dw_1.Find("faen_codigo	= " 	+ ls_faena 		+ " and " + &
							 	 			"espe_codigo	= " 	+ ls_especie   + " and (" + &
							 	 			"emba_codigo	= '" 	+ ls_embalaje  + "' OR " + &
							  				"faen_codalt	= " 	+ ls_faena		+ ")", 1,dw_1.RowCount())
END IF

IF NOT IsNull(ls_alter) THEN
	ll_fila = ll_fila + dw_1.Find("faen_codalt	= " 	+ ls_alter 		+ " and " + &
							 	 			"espe_codigo	= " 	+ ls_especie   + " OR " + &
							  				"faen_codigo	= " 	+ ls_alter, 1,dw_1.RowCount())
END IF

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Detalle de Faena ya fue ingresado anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_faenasembalajes.create
call super::create
end on

on w_mant_faenasembalajes.destroy
call super::destroy
end on

event open;call super::open;iuo_faena	=	Create uo_faenas

pb_lectura.TriggerEvent(Clicked!)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "INFORME DE FAENAS REALES POR EMBALAJE"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_faenasembalajes"

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

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_faenasembalajes
boolean visible = false
integer y = 0
integer width = 3945
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_faenasembalajes
boolean visible = false
integer x = 4251
integer y = 460
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_faenasembalajes
integer x = 4251
integer y = 164
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_faenasembalajes
integer x = 4251
integer y = 820
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_faenasembalajes
integer x = 4251
integer y = 640
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_faenasembalajes
integer x = 4251
integer y = 1564
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_faenasembalajes
integer x = 4251
integer y = 1180
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_faenasembalajes
integer x = 4251
integer y = 1000
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_faenasembalajes
integer y = 76
integer width = 3945
integer height = 1744
string dataobject = "dw_mant_faenasembalajes"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nulo
Date		ld_fechai, ld_fechat

ls_columna = dwo.name
SetNull(ls_nulo)

CHOOSE CASE ls_columna
	CASE "faen_codigo"
		IF Duplicado(data, ls_columna) THEN
			This.Object.faen_codigo[il_fila]	=	Integer(ls_nulo)
			RETURN 1
		END IF
		
		IF NOT iuo_faena.Existe(Integer(data), True, sqlca) THEN
			This.Object.faen_codigo[il_fila]	=	Integer(ls_nulo)
			RETURN 1
		END IF
		
	CASE "faen_codalt"
		IF Duplicado(data, ls_columna) THEN
			This.Object.faen_codalt[il_fila]	=	Integer(ls_nulo)
			RETURN 1
		END IF
		
		IF NOT iuo_faena.Existe(Integer(data), True, sqlca) THEN
			This.Object.faen_codalt[il_fila]	=	Integer(ls_nulo)
			RETURN 1
		END IF
		
	CASE "emba_codigo"
		IF Duplicado(data, ls_columna) THEN
			This.Object.emba_codigo[il_fila]	=	ls_nulo
			RETURN 1
		END IF
		
END CHOOSE
end event

