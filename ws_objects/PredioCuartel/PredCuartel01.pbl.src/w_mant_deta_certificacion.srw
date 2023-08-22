$PBExportHeader$w_mant_deta_certificacion.srw
forward
global type w_mant_deta_certificacion from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_certificacion from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2839
integer height = 1648
string title = "PLANTAS "
end type
global w_mant_deta_certificacion w_mant_deta_certificacion

type variables
DataWindowChild  idwc_predio, idwc_especie, idwc_estado, idwc_nivel, idwc_productores

uo_prodpredio			      iuo_prodpredio
uo_especie						iuo_especies
uo_spro_estadocertif			iuo_estadocertif
uo_spro_nivelcertif			iuo_nivelcertif
uo_productores             iuo_productores
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscapredio ()
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String	ls_productor, ls_predio, ls_especie, ls_nivel

ls_productor	=	String(dw_1.Object.prod_codigo[il_fila])
ls_predio	=	String(dw_1.Object.prpr_codigo[il_fila])
ls_especie		 =	String(dw_1.Object.espe_codigo[il_fila])
ls_nivel = String(dw_1.Object.nice_codigo[il_fila])

CHOOSE CASE as_Columna
		
 	CASE "prod_codigo"
		ls_productor =	as_Valor	
			
  	CASE "prpr_codigo"
		ls_predio  	 =	as_Valor	
		
	CASE "espe_codigo"
		ls_especie	 =	as_Valor		
	
	CASE "nice_codigo"
		ls_nivel	 =	as_Valor	

END CHOOSE

	
	ll_fila	= istr_mant.dw.Find("prod_codigo = " + ls_productor + " AND " + &
								"prpr_codigo = " + ls_predio + " AND " + &
								"espe_codigo = " + ls_especie + " AND " + &
								"nice_codigo = " + ls_nivel + " ", &	
								1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public subroutine buscapredio ();IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	Messagebox("Atención","Debe Ingresar Productor Previamente")
	dw_1.SetFocus()
	RETURN
ELSE
	istr_busq.argum[2] = String(dw_1.Object.prod_codigo[il_fila])
	
	OpenWithParm(w_busc_prodpredio, istr_busq)

	istr_busq	= Message.PowerObjectParm

	IF istr_busq.argum[1] <> "" THEN
		dw_1.Object.prpr_codigo[il_fila] = Long(istr_busq.argum[1])
		dw_1.Object.prpr_nombre[il_fila] = istr_busq.argum[3]
	END IF
END IF
end subroutine

on w_mant_deta_certificacion.create
call super::create
end on

on w_mant_deta_certificacion.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "prpr_codigo"))
ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "espe_codigo"))
ias_campo[3] = String(dw_1.GetItemNumber(il_fila, "prec_codigo"))
ias_campo[4] = String(dw_1.GetItemNumber(il_fila, "nice_codigo"))
ias_campo[5] = String(dw_1.GetItemDate(il_fila, "prce_fecaud"))
ias_campo[6] = String(dw_1.GetItemDate(il_fila, "prce_feccer"))
ias_campo[7] = String(dw_1.GetItemDate(il_fila, "prce_fecexp"))

If Not istr_mant.Agrega And Not istr_mant.Borra Then
		dw_1.Object.prod_codigo.Protect	= 1
		dw_1.Object.prpr_codigo.Protect	= 1
		dw_1.Object.prpr_nombre.Protect	= 1
		dw_1.Object.espe_codigo.Protect	= 1
		dw_1.Object.nice_codigo.Protect	= 1
		dw_1.Object.prod_codigo.Color	= RGB(255,255,255)
		dw_1.Object.prpr_codigo.Color		= RGB(255,255,255)
		dw_1.Object.prpr_nombre.Color	= RGB(255,255,255)
		dw_1.Object.espe_codigo.Color	= RGB(255,255,255)
		dw_1.Object.nice_codigo.Color		= RGB(255,255,255)
		dw_1.Object.prod_codigo.BackGround.Color		= 553648127
		dw_1.Object.prpr_codigo.BackGround.Color		= 553648127
		dw_1.Object.prpr_nombre.BackGround.Color	= 553648127
		dw_1.Object.espe_codigo.BackGround.Color	= 553648127
		dw_1.Object.nice_codigo.BackGround.Color		= 553648127
End If




end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "prpr_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "prec_codigo", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "nice_codigo", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "prce_fecaud", Date(ias_campo[5]))
	dw_1.SetItem(il_fila, "prce_feccer", Date(ias_campo[6]))
	dw_1.SetItem(il_fila, "prce_fecexp", Date(ias_campo[7]))
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;String	ls_mensaje, ls_colu[]
Long		li_cont

IF Isnull(dw_1.Object.prpr_codigo[il_fila]) OR dw_1.Object.prpr_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Predio"
	ls_colu[li_cont]	= "prpr_codigo"
END IF


IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.prec_codigo[il_fila]) OR dw_1.Object.prec_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nEstado de Certificación"
	ls_colu[li_cont]	= "prec_codigo"
END IF

IF Isnull(dw_1.Object.nice_codigo[il_fila]) OR dw_1.Object.nice_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNivel de Certificación"
	ls_colu[li_cont]	= "nice_codigo"
END IF


IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;call super::open;iuo_prodpredio			= 	CREATE 	uo_prodpredio			
iuo_especies			=	CREATE	uo_especie					
iuo_estadocertif		=	CREATE	uo_spro_estadocertif			
iuo_nivelcertif			=	CREATE	uo_spro_nivelcertif
iuo_productores  		=  CREATE	uo_productores

istr_mant.argumento[1] = '0'

dw_1.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(-1)

dw_1.GetChild("prpr_codigo", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.Retrieve(0)

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()

dw_1.GetChild("prec_codigo", idwc_estado)
idwc_estado.SetTransObject(sqlca)
idwc_estado.Retrieve()

dw_1.GetChild("nice_codigo", idwc_nivel)
idwc_nivel.SetTransObject(sqlca)
idwc_nivel.Retrieve()

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_certificacion
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_certificacion
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_certificacion
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_certificacion
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_certificacion
integer x = 2542
integer y = 352
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_certificacion
integer x = 2537
integer y = 136
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_certificacion
integer x = 2537
integer y = 568
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_certificacion
integer x = 123
integer y = 80
integer width = 2222
integer height = 1332
string dataobject = "dw_mant_certificacion"
end type

event dw_1::itemchanged;Long		ll_fila, ll_null
Integer	li_Zona
String	ls_campo, ls_busca, ls_null

SetNull(ls_null)

ls_campo = GetColumnName()

CHOOSE CASE ls_campo
	CASE "prod_codigo"
		IF Duplicado(ls_campo,Data) THEN
    		This.SetItem(il_fila, ls_Campo, Long(ls_Null))
			RETURN 1
			ELSEIF NOT iuo_productores.Existe(Long(Data),True,Sqlca) THEN
				This.SetItem(il_fila, "prod_codigo", Long(ls_null))
				RETURN 1
			ELSE
				dw_1.GetChild("prpr_codigo", idwc_predio)
				idwc_predio.Retrieve(Long(data))
				dw_1.SetItem(il_fila, "prpr_codigo", Long(ls_null))
							
				istr_mant.argumento[1] = String(Data)
		END IF	
		
	CASE "prpr_codigo"
         IF Duplicado(ls_campo,Data) THEN
    			This.SetItem(il_fila, ls_Campo, Long(ls_Null))
				RETURN 1
				ELSEIF istr_mant.argumento[1] = '0' THEN
					Messagebox("Atención","Debe Seleccionar un Productor Previamente")
					This.SetItem(il_fila, "prpr_codigo", Long(ls_null))
					dw_1.SetColumn("prod_codigo")
					RETURN 1
				ELSEIF Not iuo_prodpredio.Existepredioprod(Long(istr_mant.argumento[1]),Long(data),True,Sqlca) THEN
					This.SetItem(il_fila, "prpr_codigo", Long(ls_null))
					This.SetItem(il_fila, "prpr_nombre", ls_null)
					RETURN 1
				ELSE
					dw_1.Object.prpr_nombre[il_fila] = iuo_prodpredio.nombre
			END IF
						
	CASE "espe_codigo"
		IF Duplicado(ls_campo,Data) THEN
    		This.SetItem(il_fila, ls_Campo, Integer(ls_Null))
			RETURN 1
			ELSEIF NOT iuo_especies.existe(Integer(data),TRUE,sqlca)	THEN
				This.SetItem(il_fila, "espe_codigo", Integer(ls_Null))
				RETURN 1
		END IF
		
	CASE "prec_codigo"
		IF NOT iuo_estadocertif.existe(Integer(data),TRUE,sqlca)	THEN
			This.SetItem(il_fila, "prec_codigo", Integer(ls_Null))
			RETURN 1
		END IF
		
	CASE "nice_codigo"
		IF Duplicado(ls_campo,Data) THEN
    		This.SetItem(il_fila, ls_Campo, Integer(ls_Null))
			RETURN 1
		ELSEIF NOT iuo_nivelcertif.existe(Integer(data),TRUE,sqlca)	THEN
			This.SetItem(il_fila, "nice_codigo", Integer(ls_Null))
			RETURN 1
		END IF	

END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event

event dw_1::clicked;call super::clicked;CHOOSE CASE dwo.Name
	CASE "buscapredio"
		buscapredio()
END CHOOSE

end event

