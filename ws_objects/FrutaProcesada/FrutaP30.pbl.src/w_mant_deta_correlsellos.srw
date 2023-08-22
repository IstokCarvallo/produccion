$PBExportHeader$w_mant_deta_correlsellos.srw
forward
global type w_mant_deta_correlsellos from w_mant_detalle
end type
end forward

global type w_mant_deta_correlsellos from w_mant_detalle
integer width = 2839
integer height = 1172
string title = "CORRELATIVOS SELLOS"
end type
global w_mant_deta_correlsellos w_mant_deta_correlsellos

type variables
DataWindowChild	 dw_planta

String	is_rut, is_rutdig, ls_column, ls_column1
end variables

forward prototypes
public function boolean duplicado (string columna, integer valor)
end prototypes

public function boolean duplicado (string columna, integer valor);Long        ll_fila                              
Integer		li_tipodoc

li_tipodoc 		= Integer(valor)

CHOOSE CASE columna
	case "sell_secuen"
		li_tipodoc = Integer(valor)
	
END CHOOSE

ll_fila  = istr_mant.dw.Find("sell_secuen = " + String(li_tipodoc),1,istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN 
	MessageBox("Error","Código ya fue ingresado",Information!, OK!)
   RETURN True
ELSE

	RETURN False
END IF

end function

on w_mant_deta_correlsellos.create
call super::create
end on

on w_mant_deta_correlsellos.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.plde_codigo[il_fila])
ias_campo[2] = String(dw_1.Object.sell_secuen[il_fila])
ias_campo[3] = String(dw_1.Object.sell_inicio[il_fila])
ias_campo[4] = String(dw_1.Object.sell_termin[il_fila])
ias_campo[5] = String(dw_1.Object.sell_fregis[il_fila])
ias_campo[6] = String(dw_1.Object.sell_vigenc[il_fila])

IF istr_Mant.Agrega THEN dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))

IF Not istr_mant.Agrega AND Not istr_mant.Borra THEN
	dw_1.Object.plde_codigo.Protect				=	1
	dw_1.Object.plde_codigo.Color					=	0
	dw_1.Object.plde_codigo.BackGround.Color	=	553648127
END IF

dw_1.SetFocus()
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	
	dw_1.SetItem(il_fila, "plde_codigo",Integer(ias_campo[1]))
   dw_1.SetItem(il_fila, "sell_secuen",Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "sell_inicio",Long(ias_campo[3]))
	dw_1.SetItem(il_fila, "sell_termin",Long(ias_campo[4]))
	dw_1.SetItem(il_fila, "sell_fregis",Date(ias_campo[5]))
	dw_1.SetItem(il_fila, "sell_vigenc",Integer(ias_campo[6]))

END IF
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

//IF Isnull(dw_1.GetItemNumber(il_fila, "sell_secuen")) OR dw_1.GetItemNumber(il_fila, "sell_secuen") = 0 THEN
//	li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nOrden"
//	ls_colu[li_cont]	= "sell_secuen"
//END IF

IF Isnull(dw_1.Object.sell_inicio[il_fila]) OR (dw_1.Object.sell_inicio[il_fila]) = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCorrelativo Inicial"
	ls_colu[li_cont]	= "sell_inicio"
END IF

IF Isnull(dw_1.Object.sell_termin[il_fila]) OR (dw_1.Object.sell_termin[il_fila]) = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCorrelativo Final"
	ls_colu[li_cont]	= "sell_termin"
END IF

IF Isnull(dw_1.Object.sell_fregis[il_fila])  THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nFecha de Registro"
	ls_colu[li_cont]	= "sell_fregis"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))

end event

event open;x	= 100
y	= 450
PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.GetChild("plde_codigo",dw_planta)
dw_planta.SetTransObject(sqlca)

IF dw_planta.Retrieve(Integer(istr_mant.argumento[1])) = 0 THEN dw_planta.InsertRow(0)

end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_correlsellos
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_correlsellos
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_correlsellos
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_correlsellos
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_correlsellos
integer x = 2418
integer y = 436
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_correlsellos
integer x = 2418
integer y = 164
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_correlsellos
integer x = 2418
integer y = 712
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_correlsellos
integer x = 87
integer y = 136
integer width = 2167
integer height = 820
string dataobject = "dw_mant_correlsellos"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Null
Integer	li_bodadm, li_planta, li_tipo
Long		ll_inicio, ll_termin, ll_ultimo
Date		ld_fecha, ld_Null

SetNull(ls_Null)
SetNull(ld_Null)

ls_columna 	= 	dwo.Name

li_planta	=	Integer(istr_mant.argumento[1])

ll_inicio	=	dw_1.Object.sell_inicio[il_fila]
ll_termin	=	dw_1.Object.sell_termin[il_fila]

CHOOSE CASE ls_columna
	CASE "sell_secuen"
		IF Duplicado(ls_columna, Integer(data)) THEN
	   	This.SetItem(il_fila, ls_columna, Integer(ls_Null))
		  	RETURN 1
		END IF
		
	CASE "sell_inicio"
		ll_inicio	=	Long(data)
		IF Not Isnull(ll_termin) THEN
			IF ll_termin < ll_inicio THEN
				dw_1.SetItem(il_fila, "sell_inicio", 0)
				RETURN 1
			END IF
		END IF

      select max(sell_termin) into :ll_ultimo
		  from dba.correlsellos
		  where plde_codigo=:li_planta;
		  
		  if IsNull(ll_ultimo) then
			   ll_ultimo=0
		  end if
		
		IF ll_inicio < ll_ultimo THEN
			MessageBox("Error","Dato Inicial No Puede Ser Menor al Ultimo Registrado",Information!, Ok!)
			dw_1.SetItem(il_fila, "sell_inicio", ll_ultimo+1)
			RETURN 1
		END IF

	CASE "sell_termin"
		ll_termin	=	Long(data)
		IF ll_termin < ll_inicio THEN
			dw_1.SetItem(il_fila, "sell_termin", 0)
			RETURN 1
		END IF
		
	CASE "sell_fregis"
		ld_fecha	=	Date(data)
		IF ld_fecha < Today() THEN
			MessageBox("Error","Fecha Registro No Puede Ser Menor a la Fecha del Dia",Information!, Ok!)
			dw_1.SetItem(il_fila, "sell_fregis", Today())
			RETURN 1
		END IF
		
END CHOOSE
end event

