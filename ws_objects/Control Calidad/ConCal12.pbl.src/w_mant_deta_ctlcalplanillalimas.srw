$PBExportHeader$w_mant_deta_ctlcalplanillalimas.srw
forward
global type w_mant_deta_ctlcalplanillalimas from w_mant_detalle_csd
end type
type tab_1 from tab within w_mant_deta_ctlcalplanillalimas
end type
type tabpage_1 from userobject within tab_1
end type
type dw_embalaje from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_embalaje dw_embalaje
end type
type tabpage_2 from userobject within tab_1
end type
type dw_calidad from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_calidad dw_calidad
end type
type tabpage_3 from userobject within tab_1
end type
type dw_condicion from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_condicion dw_condicion
end type
type tabpage_4 from userobject within tab_1
end type
type dw_resolucion from uo_dw within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_resolucion dw_resolucion
end type
type tabpage_5 from userobject within tab_1
end type
type dw_id from uo_dw within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_id dw_id
end type
type tab_1 from tab within w_mant_deta_ctlcalplanillalimas
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
end type
type dw_7 from datawindow within w_mant_deta_ctlcalplanillalimas
end type
end forward

global type w_mant_deta_ctlcalplanillalimas from w_mant_detalle_csd
integer width = 3497
integer height = 1960
boolean controlmenu = true
tab_1 tab_1
dw_7 dw_7
end type
global w_mant_deta_ctlcalplanillalimas w_mant_deta_ctlcalplanillalimas

type variables
Integer	ii_Punuba, ii_Punura, ii_Pununi
String	is_Columna
Integer ii_cliente, ii_plde,ii_cclo,ii_ccpe,ii_valor

DataWindowChild idwc_clientes,idwc_plantas
DataWindow	dw_2, dw_3, dw_4, dw_5,dw_6


end variables

forward prototypes
public function boolean valiracimo (string columna, string valor)
public subroutine sumaformaracimo (string as_columna, string valor, integer numero)
public function boolean sumpesoracim (integer ai_tipo, string as_valor)
public function boolean sumaracimo (integer ai_tipo, string as_valor)
public function boolean cienxciento (string columna, string valor)
public subroutine datosembalaje (string columna, string valor)
public function boolean datosresolucion (string columna, string valor)
public function string datosresolucioncaja (string columna, string valor)
public function boolean duplicado (string campo, integer tipo)
public subroutine pesoproba (string columna, string valor)
public subroutine promediobayas (string columna, string valor)
public function boolean resulcaj (integer ai_resul)
public function boolean sumadeshi (integer ai_tipo, string as_valor)
public function boolean existedañoinsecto (string data)
public function boolean existenvaloresracimo (integer ai_punura, integer ai_punuba, integer ai_pununi)
public function boolean cienporciento (string columna, string valor)
public function integer validapeso (string columna, string valor)
public function boolean existe_causal (integer ai_causal)
public function boolean existe_matextraña (string data)
public function boolean calificacion (string columna, string valor, boolean ab_boolean)
protected function boolean validacalificacion (string columna)
public function boolean valida_causal (string as_columna, integer ai_valor)
public function long existepallet (integer ai_cliente, integer ai_planta, long al_lote, long al_pallet)
public function boolean causales (integer ai_causa, integer ai_especie)
public function boolean causales2 (integer ai_causa, integer ai_especie)
end prototypes

public function boolean valiracimo (string columna, string valor);//Integer	li_valor1, li_valor2, li_valor3, li_suma
//String	ls_Null
//
//SetNull(ls_Null)
//
//CHOOSE CASE Columna
//		
//	CASE "ccpd_punuba"
//		li_valor1	=	Integer(valor)
//		li_Suma		=	li_valor1
//		
//	CASE "ccpd_pununi"
//		
//		li_valor2	=	Integer(valor)
//		li_suma     =  li_valor1 + li_valor2 
//
////	CASE "ccpd_punura"
////		li_valor3 	=	Tab_1.TabPage_3.dw_condicion.Object.ccpd_punura[il_fila]		
////	
//END CHOOSE
//		IF li_suma > 0  THEN 
//			Messagebox("Atención","Debe Ingresar obligatoriamente Nro de Racimo")
//			dw_4.Setfocus()
//			RETURN FALSE
//		END IF 	
//
//
//RETURN TRUE
//

return false
end function

public subroutine sumaformaracimo (string as_columna, string valor, integer numero);//Integer	li_Valor1, li_Valor2, li_Suma
//
//IF Numero	=	1	THEN		
//	
//	CHOOSE CASE	as_Columna
//		CASE	"ccpd_forade"
//			li_Valor1	=	Integer(Valor)
//
//			li_Valor2		=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_foraap[il_Fila]
//			
//		CASE	"ccpd_foraap"
//			li_Valor1		=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_forade[il_Fila]
//			li_Valor2	=	Integer(Valor)		
//			
//	END CHOOSE
//	
//ELSEIF Numero	=	2	THEN
//	
//	CHOOSE CASE as_Columna
//			
//		CASE	"ccpd_pesras"
//			li_Valor1	=	Integer(Valor)
//			li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesrab[il_Fila]
//		CASE	"ccpd_pesrab"
//			li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesras[il_Fila]
//			li_Valor2	=	Integer(Valor)
//			
//	END CHOOSE	
//	
//END IF
//
//
//li_Suma	=	li_Valor1	+	li_Valor2
//
//IF li_Suma	>	100 THEN
//	
//	MessageBox("Atención","Sumatoria de Forma de Racimo~r" +&
//				  " no debe exceder 100 ")						
//END IF						
end subroutine

public function boolean sumpesoracim (integer ai_tipo, string as_valor);//Decimal 	ldc_sobre, ldc_bajo, ldc_Suma
//
//ldc_Sobre		=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesras[il_Fila]
//ldc_Bajo			=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesrab[il_Fila]		
//
//
//CHOOSE CASE ai_tipo
//		
//	CASE 1
//		ldc_Sobre	=	Dec(as_Valor)
//		
//	CASE 2
//		ldc_Bajo		=	Dec(as_valor)	
//		
//END CHOOSE
//
//IF IsNull(ldc_Sobre)	THEN ldc_Sobre  = 0
//IF IsNull(ldc_bajo)	THEN ldc_Bajo	= 0 
//
//ldc_Suma	=	ldc_Sobre + ldc_Bajo
//
//IF ldc_Suma > 100 THEN	
//	MessageBox("Atención","La Sumatoria del Peso de Racimo~r" +&
//							"no debe ser mayor a 100")
//	RETURN False
//END IF					
//
//RETURN True


return false
end function

public function boolean sumaracimo (integer ai_tipo, string as_valor);//
//Decimal	ldc_deforme, ldc_apretado,ldc_Suma
//
//ldc_deforme	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_forade[il_Fila]
//ldc_apretado	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_foraap[il_Fila]		
//
//
//CHOOSE CASE ai_tipo
//		
//	CASE 1
//		ldc_deforme	=	Integer(as_valor)
//		
//	CASE 2
//		ldc_apretado	=	Integer(as_valor)	
//		
//END CHOOSE
//
//IF IsNull(ldc_Deforme)  THEN ldc_Deforme  = 0
//IF IsNull(ldc_Apretado) THEN ldc_Apretado = 0 
//
//ldc_Suma	=	ldc_Deforme + ldc_Apretado 
//
//IF ldc_Suma > 100 THEN	
//	MessageBox("Atención","La Sumatoria de la Forma de Racimo  ~r" +&
//							"no debe ser mayor a 100")
//	RETURN False
//END IF					
//
//RETURN True

return false
end function

public function boolean cienxciento (string columna, string valor);Integer	li_valor1, li_suma
String	ls_Null

SetNull(ls_Null)

CHOOSE CASE Columna

	CASE "cpvd_color3"
		li_valor1	=	Integer(valor)
		li_Suma		=	li_Valor1
		IF li_suma > 100 THEN			
			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
			dw_3.SetFocus()
			RETURN FALSE
		END IF
END CHOOSE

RETURN TRUE

end function

public subroutine datosembalaje (string columna, string valor);Integer	li_v5, li_v7, li_v8, li_v9
String	ls_v1, ls_v2, ls_v3, ls_v4, ls_v6
Dec{2}	ld_v9, ld_v5

ls_v1				=	dw_2.Object.cpvd_paleti[il_fila]
ls_v2				=	dw_2.Object.cpvd_rotula[il_fila]
ls_v3				=	dw_2.Object.cpvd_materi[il_fila]
ls_v4				=	dw_2.Object.cpvd_empaq[il_fila]
li_v5				=	dw_2.Object.cpvd_pesone[il_fila]
ls_v6				=	dw_2.Object.cpvd_aparie[il_fila]

CHOOSE CASE Columna
	CASE "cpvd_paleti"
		ls_v1	=	valor
		IF ls_v1 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Paletizaje.", StopSign!, Ok!)					
			dw_2.SetFocus()
		END IF

	CASE "cpvd_rotula"
		ls_v2	=	valor
		IF ls_v2 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Rotulación.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "cpvd_materi"
		ls_v3	=	valor
		IF ls_v3 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Materiales.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "cpvd_empaq"
		ls_v4 =	valor
		IF ls_v4 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Empaque.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "cpvd_pesone"
		ld_v5 =	Dec(valor)
		IF ld_v5 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Peso Neto.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "cpvd_aparie"
		ls_v6 =	valor
		IF ls_v6 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Apariencia.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
END CHOOSE

end subroutine

public function boolean datosresolucion (string columna, string valor);String	ls_v1, ls_v2, ls_v3, ls_v4

ls_v1		=	dw_5.Object.cpvd_resemb[il_fila]
ls_v2		=	dw_5.Object.cpvd_rescal[il_fila]
ls_v3		=	dw_5.Object.cpvd_rescon[il_fila]

CHOOSE CASE Columna

	CASE "cpvd_resemb"
		ls_v1	=	valor
		IF ls_v1 = '' OR ls_v1<>'B' AND ls_v1 <> 'R' AND ls_v1<>'M' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Calificación Embalaje B, R, o M.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False
		END IF

	CASE "cpvd_rescal"
		ls_v2	=	valor
		IF ls_v2 = '' OR ls_v2<>'B' AND ls_v2<>'R' AND ls_v2<>'M'THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Calificación Calidad B, R, o M.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False
		END IF

	CASE "cpvd_rescon"
		ls_v3	=	valor
		IF ls_v3 = '' OR ls_v3<>'B' AND ls_v3<>'R' AND ls_v3<>'M'THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Calificación Condición B, R, o M.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False			
		END IF
		
END CHOOSE

RETURN True


end function

public function string datosresolucioncaja (string columna, string valor);String	ls_v1, ls_v2, ls_v3, ls_v4, ls_juntos

ls_v1		=	dw_5.Object.cpvd_resemb[il_fila]
ls_v2		=	dw_5.Object.cpvd_rescal[il_fila]
ls_v3		=	dw_5.Object.cpvd_rescon[il_fila]

CHOOSE CASE Columna

	CASE "cpvd_resemb"
		ls_v1	=	valor

	CASE "cpvd_rescal"
		ls_v2	=	valor

	CASE "cpvd_rescon"
		ls_v3	=	valor
		
END CHOOSE

ls_juntos	=	ls_v1+ls_v2+ls_v3

IF ls_juntos = 'BBB' THEN 
	ls_v4 = 'A'
ELSEIF ls_juntos ='BBR' OR ls_juntos ='RBB' OR ls_juntos ='BRB' THEN
	ls_v4 = 'A'
ELSEIF ls_v1 = 'M' OR ls_v2 = 'M' OR ls_v3 = 'M' THEN
	ls_v4 = 'O'
ELSE 
	ls_v4 = 'Z'
END IF

RETURN ls_v4

end function

public function boolean duplicado (string campo, integer tipo);//Long        ll_fila
//String      ls_numero
//
//ls_numero	=	String(dw_1.GetItemNumber(il_fila,"rpcf_numero"))
//
//CHOOSE CASE tipo
//	case 1
//		ls_numero	=	campo
//
//END CHOOSE
//
//ll_fila = dw_1.Find("rpcf_numero = " + ls_numero, 1,dw_1.RowCount())
//
//IF ll_fila > 0 and ll_fila <> il_fila THEN
//	MessageBox("Error","Número de Párrafo ya fue ingresado anteriormente",Information!, OK!)
//   RETURN True
//ELSE
//	RETURN False
//END IF
//
return false
end function

public subroutine pesoproba (string columna, string valor);//Dec{1}	ld_promba
//
////ld_promba	=	dw_3.Object.ccpd_promba[il_fila]
//
//CHOOSE CASE Columna
//		
//	CASE "ccpd_promba"
//		ld_promba 		=	Dec(valor)
//		IF ld_promba	= 0 or ld_promba >20 THEN
//			MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio correspondiente .", StopSign!, Ok!)
//			ii_valor=6
//			dw_3.SetFocus()
//		END IF	
//
//END CHOOSE
//
end subroutine

public subroutine promediobayas (string columna, string valor);//Dec{2}	li_valor1
//String	ls_Null
//
//SetNull(ls_Null)
//
//li_valor1		=	dw_3.Object.ccpd_promba[il_fila]
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_promba"
//		li_valor1	=	Dec(valor)
//
//END CHOOSE
//
//
//IF li_valor1 = 0 THEN	
//	MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio de Bayas.", StopSign!, Ok!)
//	dw_3.SetFocus()
//END IF
//
//
end subroutine

public function boolean resulcaj (integer ai_resul);//Integer	li_Resul, li_Existe 
//
//li_Resul	=	ai_resul
//
//SELECT Count(*)
//INTO	 :li_Existe 
//FROM	 dbo.ctlcaldanoespecie
//WHERE	 espe_codigo = 11
//AND    clie_codigo = 81
//AND    ccda_secuen = :li_Resul;
//
//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
//ELSEIF li_Existe	>	0	THEN
//	RETURN TRUE
//ELSE
//	MessageBox("Atención","Causal digitado, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
//END IF
//
//RETURN FALSE
//
//

return false
end function

public function boolean sumadeshi (integer ai_tipo, string as_valor);//Integer 	li_ValLev, li_ValMod, li_ValAlt, li_Suma
//
//li_ValLev	=	Tab_1.TabPage_3.dw_Condicion.Object.ccpd_deslev[il_Fila]
//li_ValMod	=	Tab_1.TabPage_3.dw_Condicion.Object.ccpd_desmod[il_Fila]		
//li_ValAlt	=	Tab_1.TabPage_3.dw_Condicion.Object.ccpd_desalt[il_Fila]
//
//CHOOSE CASE ai_tipo
//		
//	CASE 1
//		li_ValLev	=	Integer(as_valor)
//		
//	CASE 2
//		li_ValMod	=	Integer(as_valor)
//		
//	CASE 3
//		li_ValAlt	=	Integer(as_valor)
//		
//END CHOOSE
//
//IF IsNull(li_ValLev) THEN li_ValLev = 0
//IF IsNull(li_ValMod) THEN li_ValMod = 0 
//IF	IsNull(li_ValAlt) THEN li_ValAlt = 0
//
//li_Suma	=	li_ValLev + li_ValMod + li_ValAlt		
//
//IF li_Suma > 100 THEN	
//	MessageBox("Atención","La Sumatoria de la Deshidratación ~r" +&
//							"no debe ser mayor a 100",StopSign!)
//	RETURN False
//END IF						
//
//RETURN True

return false
end function

public function boolean existedañoinsecto (string data);//Integer	li_Daño	, li_Existe
//
//li_Daño	=	Integer(Data)
//
//SELECT Count(*)
//INTO	 :li_Existe
//FROM	 dbo.ctlcaldanoespecie
//WHERE	 espe_codigo = 11
//AND    ccfa_codigo = 30
//AND    ccsf_codigo = 18
//AND    ccda_secuen = :li_Daño	;
//
//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
//ELSEIF	li_Existe	>	0	THEN
//	RETURN TRUE
//ELSE
//	MessageBox("atención","Daño digitado no corresponde a Insectos")
//END IF
//
//RETURN FALSE
//
return false
end function

public function boolean existenvaloresracimo (integer ai_punura, integer ai_punuba, integer ai_pununi);//String	li_Nula
//
//SetNull(li_Nula)
//
//Tab_1.TabPage_3.dw_condicion.accepttext()
//
//IF ai_punuba <> 0 OR ai_pununi <> 0 AND ai_punura	=	0 THEN	
//	MessageBox("Atención","Debe Ingresar el Número de Racimos", StopSign!)	
//	RETURN False
//END IF 	
//
//Return True
//
//
//
//

return false
end function

public function boolean cienporciento (string columna, string valor);Integer	li_valor1, li_valor2, li_suma
String	ls_Null

SetNull(ls_Null)

CHOOSE CASE Columna

	CASE "cpvd_calmay"
		li_valor1	=	Integer(valor)
		li_Suma		=	li_Valor1	
		IF li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
		
	CASE "cpvd_calmen"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.cpvd_calmay[il_Fila]
		li_Valor2	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2
		IF  li_suma > 100 OR	li_Suma	<	100	THEN
				MessageBox("Error de Consistencia", "La Suma de Calibre debe dar siempre 100.", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
END CHOOSE

RETURN TRUE
end function

public function integer validapeso (string columna, string valor);Dec{2}	ld_valor1, ld_PesoMax, ld_PesoMin, ld_Pesoneto
String		ls_Null, ls_embalaje
Integer	li_Cliente

SetNull(ls_Null)

ld_valor1		=	Dec(valor)
ls_embalaje = istr_mant.argumento[12]
li_Cliente		= Integer(istr_mant.argumento[7])
		
SELECT	en.enva_pesone
	INTO 	:ld_PesoNeto
 	FROM 	dbo.embalajesprod as em, dbo.envases as en
	WHERE em.emba_codigo = :ls_embalaje
	AND	em.clie_codigo = :li_Cliente 
	AND	em.enva_tipoen = en.enva_tipoen
	AND	em.enva_codigo = en.enva_codigo;
	
IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Embalajes")
ELSEIF	ld_PesoNeto = 0 THEN	
	MessageBox("Atención","Envase asociado al Código Embalaje No tiene Ingresado Peso Neto")
	ld_valor1=0
	ii_valor =0
ELSE
	ld_PesoMax	=	ld_PesoNeto + ld_PesoNeto*.5
	ld_PesoMin	=	ld_PesoNeto - ld_PesoNeto*.5
	 IF ld_valor1 > ld_PesoMax OR ld_valor1 < ld_PesoMin THEN
		MessageBox("Error de Consistencia", " Peso fuera de rango.", StopSign!, Ok!)
		dw_3.SetFocus()
	 	 ld_valor1=0
	 	 ii_valor =0
 	 END IF
END IF	

RETURN ld_valor1


end function

public function boolean existe_causal (integer ai_causal);Integer	li_Causal, li_Existe, li_especie

li_Causal	=	ai_causal

li_especie = Integer(istr_mant.argumento[9])

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo	=	:li_especie
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	RETURN TRUE
ELSE
	MessageBox("Atención","Causal digitado, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
END IF
RETURN FALSE


return false


end function

public function boolean existe_matextraña (string data);//Integer li_matext, li_existe
//
//li_matext	=	integer(data)
//
//SELECT Count(*)
//INTO	 :li_Existe
//FROM	 dbo.ctlcaldanoespecie
//WHERE	 espe_codigo = 11
//AND    ccfa_codigo = 30
//AND    ccsf_codigo = 16
//AND    ccda_secuen = :li_matext	;
//
//
//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
//ELSEIF li_Existe	>	0	THEN
//	RETURN TRUE
//ELSE
//	MessageBox("Atención","Materia Extraña digitada, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
//END IF
//RETURN FALSE
//
//
//
//
//

return false
end function

public function boolean calificacion (string columna, string valor, boolean ab_boolean);Integer	li_cont, li_suma, li_Nula, li_Sw
String	ls_mensaje, ls_colu[]

SetNull(li_Nula)
li_suma  = 0 

dw_3.AcceptText()

IF IsNull(dw_3.object.cpvd_calmay[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.cpvd_calmay[il_fila])
END IF		

IF IsNull(dw_3.object.cpvd_calmen[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.cpvd_calmen[il_fila])
END IF

IF li_suma <> 100 THEN
	IF ab_Boolean THEN
		MessageBox("Error de Consistencia", "La Suma de Calibres debe dar siempre 100.", StopSign!, Ok!)
	END IF
	dw_3.SetFocus()
	Message.DoubleParm = -1	
	li_Sw	=	0
ELSE
	li_Sw = li_Sw + 1
END IF

IF li_Sw = 1 THEN
	dw_3.Object.cpvd_rescal.protect = 0
   dw_3.Object.cpvd_rescal.BackGround.Color	=	RGB(255,255,255)
	RETURN TRUE
ELSE
	dw_3.Object.cpvd_rescal.protect = 1
	dw_3.Object.cpvd_rescal.BackGround.Color	=	RGB(192,192,192)
	RETURN FALSE
END IF
end function

protected function boolean validacalificacion (string columna);String	ls_v1, ls_v2, ls_v3, ls_v4, ls_v5, ls_Nula
SetNull(ls_Nula)

ls_v1		=	dw_2.Object.cpvd_paleti[il_fila]
ls_v2		=	dw_2.Object.cpvd_rotula[il_fila]
ls_v3		=	dw_2.Object.cpvd_materi[il_fila]
ls_v4		=	dw_2.Object.cpvd_empaq[il_fila]
ls_v5		=	dw_2.Object.cpvd_aparie[il_fila]

CHOOSE CASE Columna
		
CASE "cpvd_resemb"
		IF ls_v1 = 'M' OR ls_v2 = 'M' OR ls_v3 = 'M' OR ls_v4 = 'M' OR ls_v5 = 'M'  THEN
			MessageBox("Error de Calificación", "Calificación Embalaje debe ser Mala.", StopSign!, Ok!)
	
			RETURN TRUE
		ELSE
			RETURN FALSE
		END IF
END CHOOSE


end function

public function boolean valida_causal (string as_columna, integer ai_valor);Integer li_causa1, li_causa2, li_causa3
String ls_nombre

CHOOSE CASE as_columna
	CASE 'cpvd_cobjem'
		li_causa1 = 100
		li_causa2 = 199
		ls_nombre = 'Embalaje'
	CASE 'cpvd_cobjca'
		li_causa1 = 300
		li_causa2 = 399
		ls_nombre = 'Calidad'
	CASE 'cpvd_cobjco'
		li_causa1 = 400
		li_causa2 = 499
		ls_nombre = 'Condición'
END CHOOSE
li_causa3 = 550

IF ai_valor < li_causa1 OR ai_valor > li_causa2 AND ai_valor <> li_causa3 THEN 
	MessageBox("ERROR","Causal de Objeción no corresponde a~ " + ls_nombre,StopSign!)
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

public function long existepallet (integer ai_cliente, integer ai_planta, long al_lote, long al_pallet);Integer	li_existepallet
Long		ll_ccajas
string  ls_planilla

SetNull(ll_ccajas)

SELECT list(distinct ccpv_numero)
   INTO :ls_planilla
  FROM dbo.ctlcalplaniverifideta
WHERE cpvd_numpal = :al_pallet;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla dbo.ctlcalplaniverifideta")
//ELSEIF ls_planilla <> "" then
//	MessageBox("Atención","Número de pallet ya existe en planilla(s) "+string(ls_planilla))
ELSE
	SELECT count()
		INTO :li_existepallet
	  FROM dbo.palletfrutahisto
	 WHERE clie_codigo	=	:ai_cliente
		AND plde_codigo	=	:ai_planta
		AND paen_numero	=	:al_pallet
		AND pafr_tipdoc	=	1;		
		
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Historica de Pallet")
	ELSEIF	li_existepallet = 0	THEN	
		MessageBox("Atención","Número de pallet no existe")
	ELSE
		SELECT sum(isnull(pafh_ccajas,0))
			INTO :ll_ccajas
		  FROM dbo.palletfrutahisto
		 WHERE clie_codigo		=	:ai_cliente
			AND plde_codigo	=	:ai_planta
			AND paen_numero	=	:al_pallet
			AND pafr_tipdoc		=	1
			AND pafh_nrlote	=	:al_lote;
			
		IF sqlca.sqlcode	=	-1	THEN
			F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Historica de Pallet")
		ELSEIF	isnull(ll_ccajas)	THEN	
			MessageBox("Atención","Número de pallet no tiene relación con número de lote")
		END IF
	END IF	
END IF
RETURN ll_ccajas
end function

public function boolean causales (integer ai_causa, integer ai_especie);Integer	li_Causal, li_Existe ,li_especie
Boolean	lb_Retorno	
li_Causal	=	ai_causa
li_especie = ai_especie


SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo	=	:li_especie
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	lb_Retorno	=	True
ELSE
	MessageBox("Atención","Causal digitada, no se ha creado ",StopSign!, Ok!)
END IF
RETURN lb_Retorno


end function

public function boolean causales2 (integer ai_causa, integer ai_especie);Integer	li_Causal, li_Existe 
Boolean	lb_Retorno	
String ls_nombre
li_Causal	=	ai_causa

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo	=	:ai_especie
AND    ccda_secuen	=	:ai_Causa;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
	Return True
ELSEIF li_Existe	>	0	THEN	
	Return False
ELSE
	MessageBox("Atención","Causal digitada, no se ha creado ",StopSign!, Ok!)
	Return True
END IF
Return False


//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
//	Return True
//ELSEIF li_Existe	>	0	THEN
//	IF li_Causal >= 100  AND li_Causal <= 199 THEN
//		IF dw_2.Object.cpvd_resemb[il_fila] = 'B' THEN
//			MessageBox("Atención","No es posible ingresar causal por estar con calificación embalaje buena.",StopSign!, Ok!)
//			Return True
//		END IF	
//		Return False
//	ELSEIF li_Causal >= 300  AND li_Causal <= 399 THEN
//		IF dw_2.Object.cpvd_rescal[il_fila] = 'B' THEN
//			MessageBox("Atención","No es posible ingresar causal por estar con calificación calidad buena.",StopSign!, Ok!)
//			Return True
//		END IF	
//		Return False
//	ELSEIF li_Causal >= 400  AND li_Causal <= 499 THEN	
//		IF dw_2.Object.cpvd_rescon[il_fila] = 'B' THEN
//			MessageBox("Atención","No es posible ingresar causal por estar con calificación condición buena.",StopSign!, Ok!)
//			Return True
//		END IF	
//		Return False
//	END IF
//ELSE
//	MessageBox("Atención","Causal digitada, no se ha creado ",StopSign!, Ok!)
//	Return True
//END IF
//Return False
//

end function

on w_mant_deta_ctlcalplanillalimas.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_7=create dw_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_7
end on

on w_mant_deta_ctlcalplanillalimas.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_7)
end on

event ue_recuperadatos;call super::ue_recuperadatos;String	ls_Usuario
Integer	li_Grupo
ls_Usuario	=	Upper(Gstr_Us.Nombre)

ias_campo[54]	=	dw_1.Object.cpvd_rescaj[il_fila]	
IF ias_campo[54]	<>	'' OR Not IsNull(ias_campo[54]) THEN
	istr_mant.argumento[5]	=	istr_mant.argumento[22]

IF istr_mant.Argumento[22] ='0'OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 		
		ias_campo[1]	=	String(dw_1.Object.ccpv_numero[il_fila])
		ii_ccpe			=	Integer(ias_campo[1])
		ias_campo[2]	=	String(dw_1.Object.plde_codigo[il_fila])
		ii_plde			=	Integer(ias_campo[2])
		ias_campo[3]	=	String(dw_1.Object.clie_codigo[il_fila])
		ii_cliente		=	Integer(ias_campo[3])
		ias_campo[5]	=	String(dw_1.Object.cpvd_secuen[il_fila])
		ias_campo[7]	=	dw_1.Object.cpvd_paleti[il_fila]
		ias_campo[8]	=	dw_1.Object.cpvd_rotula[il_fila]
		ias_campo[9]	=	dw_1.Object.cpvd_materi[il_fila]
		ias_campo[10]	=	dw_1.Object.cpvd_empaq[il_fila]
		ias_campo[11]	=	String(dw_1.Object.cpvd_pesone[il_fila])
		ias_campo[12]	=	dw_1.Object.cpvd_aparie[il_fila]
		ias_campo[13]	=	String(dw_1.Object.cpvd_calmay[il_fila])
		ias_campo[14]	=	String(dw_1.Object.cpvd_calma2[il_fila])
		ias_campo[15]	=	String(dw_1.Object.cpvd_calma3[il_fila])
		ias_campo[16]	=	String(dw_1.Object.cpvd_calma4[il_fila])
		ias_campo[17]	=	String(dw_1.Object.cpvd_calma5[il_fila])
		ias_campo[18]	=	String(dw_1.Object.cpvd_calmen[il_fila])
		ias_campo[19]	=	String(dw_1.Object.cpvd_color1[il_fila])
		ias_campo[20]	=	String(dw_1.Object.cpvd_color2[il_fila])
		ias_campo[21]	=	String(dw_1.Object.cpvd_color3[il_fila])
		ias_campo[22]	=	String(dw_1.Object.cpvd_color4[il_fila])
		ias_campo[23]	=	String(dw_1.Object.cpvd_deform[il_fila])
		ias_campo[24]	=	String(dw_1.Object.cpvd_heridas[il_fila])
		ias_campo[25]	=	String(dw_1.Object.cpvd_mandor[il_fila])
		ias_campo[26]	=	String(dw_1.Object.cpvd_golsol[il_fila])
		ias_campo[27]	=	String(dw_1.Object.cpvd_residu[il_fila])
		ias_campo[28]	=	String(dw_1.Object.cpvd_mancha[il_fila])
		ias_campo[29]	=	String(dw_1.Object.cpvd_russet[il_fila])
		ias_campo[30]	=	String(dw_1.Object.cpvd_insect[il_fila])
		ias_campo[31]	=	String(dw_1.Object.cpvd_matext[il_fila])
		ias_campo[32]	=	String(dw_1.Object.cpvd_otrcal[il_fila])
		ias_campo[33]	=	String(dw_1.Object.cpvd_firpul[il_fila])
		ias_campo[34]	=	String(dw_1.Object.cpvd_herabi[il_fila])
		ias_campo[35]	=	String(dw_1.Object.cpvd_medlun[il_fila])
		ias_campo[36]	=	String(dw_1.Object.cpvd_ausped[il_fila])
		ias_campo[37]	=	String(dw_1.Object.cpvd_frubla[il_fila])
		ias_campo[38]	=	String(dw_1.Object.cpvd_pudric[il_fila])
		ias_campo[39]	=	String(dw_1.Object.cpvd_deship[il_fila])
		ias_campo[40]	=	String(dw_1.Object.cpvd_pittin[il_fila])
		ias_campo[41]	=	String(dw_1.Object.cpvd_machuc[il_fila])
		ias_campo[42]	=	String(dw_1.Object.cpvd_desgap[il_fila])
		ias_campo[43]	=	String(dw_1.Object.cpvd_otrcon[il_fila])
		ias_campo[51]	=	dw_1.Object.cpvd_resemb[il_fila]
		ias_campo[52]	=	dw_1.Object.cpvd_rescal[il_fila]
		ias_campo[53]	=	dw_1.Object.cpvd_rescon[il_fila]
		ias_campo[54]	=	dw_1.Object.cpvd_rescaj[il_fila]
		ias_campo[56]	=	String(dw_1.Object.cpvd_portot[il_fila])
		ias_campo[60]	=	String(dw_1.Object.cpvd_nummaq[il_fila])
		ias_campo[61]  =  String(dw_1.Object.cpvd_numsal[il_fila])
		ias_campo[62]  =  String(dw_1.Object.cpvd_horinp[il_fila])
		ias_campo[63]  =  String(dw_1.Object.cpvd_embala[il_fila])
	
//		if dw_5.Object.cpvd_rescaj[il_fila] = "A" and dw_5.Object.cpvd_rescom[il_fila] =  "A" then
//			dw_5.Object.cpvd_rescom.protect = 1
//			dw_5.Object.cpvd_respal.protect = 1
//		end if
		
		if dw_5.Object.cpvd_rescaj[il_fila] = "O" and dw_5.Object.cpvd_rescom[il_fila] =  "O" then
			dw_5.Object.cpvd_respal.protect = 0
		end if
		
//		if dw_5.Object.cpvd_rescaj[il_fila] = "O" and dw_5.Object.cpvd_rescom[il_fila] =  "A" then
//			dw_5.Object.cpvd_respal.protect = 1
//		end if
		
		ias_campo[70]  =  String(dw_1.Object.cpvd_numpal[il_fila])	
		
		Pb_Acepta.Enabled		=	True 
		Pb_Cancela.Enabled	=	True 		
		Pb_Salir.Enabled		=	True 
	ELSE
		istr_mant.Solo_consulta						=	True 
		Tab_1.TabPage_1.dw_Embalaje.Enabled	=	False	
		Tab_1.TabPage_2.dw_Calidad.Enabled		=	False
		Tab_1.TabPage_3.dw_Condicion.Enabled	=	False			
		Tab_1.TabPage_4.dw_resolucion.Enabled	=	False		
		Tab_1.TabPage_5.dw_id.Enabled				=	False	
		Pb_Acepta.Enabled								=	False
		Pb_Cancela.Enabled							=	False
		pb_Salir.Enabled								=	True
	END IF 
END IF

IF istr_mant.Agrega THEN
	dw_1.SetItem(il_Fila,"clie_codigo",Integer(istr_mant.Argumento[7]))
	dw_1.SetItem(il_Fila,"plde_codigo",Integer(istr_mant.Argumento[4]))
	dw_1.SetItem(il_Fila,"ccpv_numero",Long(istr_mant.Argumento[2]))
	dw_2.Object.cpvd_paleti[il_fila] = 'B'
   dw_2.Object.cpvd_rotula[il_fila] = 'B'
   dw_2.Object.cpvd_materi[il_fila] = 'B'
   dw_2.Object.cpvd_empaq[il_fila]  = 'B'
   dw_2.Object.cpvd_aparie[il_fila] = 'B'
END IF

dw_2.SetRow(il_fila)
dw_2.ScrolltoRow(il_fila)
dw_3.SetRow(il_fila)
dw_3.ScrolltoRow(il_fila)
dw_4.SetRow(il_fila)
dw_4.ScrolltoRow(il_fila)
dw_5.SetRow(il_fila)
dw_5.ScrolltoRow(il_fila)	
dw_6.SetRow(il_fila)
dw_6.ScrolltoRow(il_fila)

IF dw_1.Rowcount() > 0 THEN	
	li_Grupo = BuscaGrupo(ls_Usuario)
   
   IF (istr_mant.Argumento[22] =	'0' OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!) THEN 
		pb_acepta.Enabled		= True
		pb_cancela.Enabled	= True
	
	END IF 	
END IF 	
	 
//IF (li_Grupo	= 1 OR li_Grupo = 6) OR & /* por cambio de requermiento original 2009-01-08*/
//	(dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!)THEN 
//	dw_2.Enabled			= True	
//	dw_3.Enabled			= True	
//	dw_4.Enabled			= True	
//	dw_5.Enabled			= True	
//	dw_6.Enabled	   	= True	
//	pb_acepta.Enabled		= True	
//	pb_cancela.Enabled	= True
//	pb_salir.Enabled		= True 	
//ELSE
//	 dw_2.Enabled			= False	
//	dw_3.Enabled			= False	
//	dw_4.Enabled			= False	
//	dw_5.Enabled			= False	
//	dw_6.Enabled	   	= False	
//	pb_acepta.Enabled		= False	
//	pb_cancela.Enabled	= False
//	pb_salir.Enabled		= True 	
//	 
//END IF	
IF istr_mant.agrega = True THEN
	pb_acepta.Enabled		= True	
	pb_cancela.Enabled	= True
	pb_salir.Enabled		= True 	
	dw_2.Enabled			= True	
	dw_3.Enabled			= True	
	dw_4.Enabled			= True	
	dw_5.Enabled			= True	
	dw_6.Enabled	   	= True	
END IF
end event

event ue_deshace;call super::ue_deshace;IF istr_mant.Argumento[22] ='0'OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN
	IF UpperBound(ias_campo) > 0 THEN
		dw_1.SetItem(il_fila, "ccpv_numero", Long(ias_campo[1]))
		dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[2]))
		dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[3]))
		dw_1.SetItem(il_fila, "cpvd_secuen", Integer(ias_campo[5]))
		dw_1.SetItem(il_fila, "cpvd_paleti", ias_campo[7])
		dw_1.SetItem(il_fila, "cpvd_rotula", ias_campo[8])
		dw_1.SetItem(il_fila, "cpvd_materi", ias_campo[9])
		dw_1.SetItem(il_fila, "cpvd_empaq",  ias_campo[10])
		dw_1.SetItem(il_fila, "cpvd_pesone", Integer(ias_campo[11]))
		dw_1.SetItem(il_fila, "cpvd_aparie",  ias_campo[12])
		dw_1.SetItem(il_fila, "cpvd_calmay", ias_campo[13])
		dw_1.SetItem(il_fila, "cpvd_calma2", Integer(ias_campo[14]))
		dw_1.SetItem(il_fila, "cpvd_calma3", Integer(ias_campo[15]))
		dw_1.SetItem(il_fila, "cpvd_calma4", Integer(ias_campo[16]))
		dw_1.SetItem(il_fila, "cpvd_calma5", Integer(ias_campo[17]))
		dw_1.SetItem(il_fila, "cpvd_calmen", Integer(ias_campo[18]))
		dw_1.SetItem(il_fila, "cpvd_color1", Integer(ias_campo[19]))
		dw_1.SetItem(il_fila, "cpvd_color2", Integer(ias_campo[20]))
		dw_1.SetItem(il_fila, "cpvd_color3", Integer(ias_campo[21]))
		dw_1.SetItem(il_fila, "cpvd_color4", Integer(ias_campo[22]))
		dw_1.SetItem(il_fila, "cpvd_deform", Integer(ias_campo[23]))
		dw_1.SetItem(il_fila, "cpvd_heridas", Integer(ias_campo[24]))
		dw_1.SetItem(il_fila, "cpvd_mandor", Integer(ias_campo[25]))
		dw_1.SetItem(il_fila, "cpvd_golsol", Integer(ias_campo[26]))
		dw_1.SetItem(il_fila, "cpvd_residu", Integer(ias_campo[27]))
		dw_1.SetItem(il_fila, "cpvd_mancha", Integer(ias_campo[28]))
		dw_1.SetItem(il_fila, "cpvd_russet", Integer(ias_campo[29]))
		dw_1.SetItem(il_fila, "cpvd_insect", Integer(ias_campo[30]))
		dw_1.SetItem(il_fila, "cpvd_matext", Integer(ias_campo[31]))
		dw_1.SetItem(il_fila, "cpvd_otrcal", Integer(ias_campo[32]))
		dw_1.SetItem(il_fila, "cpvd_firpul", Integer(ias_campo[33]))
		dw_1.SetItem(il_fila, "cpvd_herabi", Integer(ias_campo[34]))
		dw_1.SetItem(il_fila, "cpvd_medlun", Integer(ias_campo[35]))
		dw_1.SetItem(il_fila, "cpvd_ausped", Integer(ias_campo[36]))
		dw_1.SetItem(il_fila, "cpvd_frubla", Integer(ias_campo[37]))
		dw_1.SetItem(il_fila, "cpvd_pudric", Integer(ias_campo[38]))
		dw_1.SetItem(il_fila, "cpvd_deship", Integer(ias_campo[39]))
		dw_1.SetItem(il_fila, "cpvd_pittin", Integer(ias_campo[40]))
		dw_1.SetItem(il_fila, "cpvd_machuc", Integer(ias_campo[41]))
		dw_1.SetItem(il_fila, "cpvd_desgap", Integer(ias_campo[42]))
		dw_1.SetItem(il_fila, "cpvd_otrcon", Integer(ias_campo[43]))
		dw_1.SetItem(il_fila, "cpvd_resemb", ias_campo[51])
		dw_1.SetItem(il_fila, "cpvd_rescal", ias_campo[52])
		dw_1.SetItem(il_fila, "cpvd_rescon", ias_campo[53])
		dw_1.SetItem(il_fila, "cpvd_rescaj", ias_campo[54])
		dw_1.SetItem(il_fila, "cpvd_portot", Integer(ias_campo[56]))
		dw_1.SetItem(il_fila, "cpvd_nummaq", Integer(ias_campo[60]))
		dw_1.SetItem(il_fila, "cpvd_numsal", Integer(ias_campo[61]))
		dw_1.SetItem(il_fila, "cpvd_horinp", Time(ias_campo[62]))
		dw_1.SetItem(il_fila, "cpvd_embala", Integer(ias_campo[63]))
		
	END IF
END IF
end event

event ue_antesguardar;Integer	li_cont, li_suma, li_Nula, li_calmay, li_calma2, li_calma3, li_calma4, li_calma5, li_calmen
String		ls_mensaje, ls_colu[]

SetNull(li_Nula)

If IsNull(dw_2.object.cpvd_paleti[il_fila]) OR dw_2.object.cpvd_paleti[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rPalletizaje"
	ls_colu[li_cont] 	= "cpvd_paleti"
End If

IF Isnull(dw_1.object.cpvd_numpal[il_fila]) or dw_1.object.cpvd_numpal[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNúmero de Pallet"
	ls_colu[li_cont] 	= "cpvd_numpal"
END IF

If IsNull(dw_2.object.cpvd_rotula[il_fila]) OR dw_2.object.cpvd_rotula[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rRotulación"
	ls_colu[li_cont] 	= "cpvd_rotula"
End If

If IsNull(dw_2.object.cpvd_materi[il_fila]) OR dw_2.object.cpvd_materi[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rMateriales"
	ls_colu[li_cont] 	= "cpvd_materi"
End If

If IsNull(dw_2.object.cpvd_empaq[il_fila]) OR dw_2.object.cpvd_empaq[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rEmpaque"
	ls_colu[li_cont] 	= "cpvd_empaq"
End If

If IsNull(dw_2.object.cpvd_pesone[il_fila]) OR dw_2.object.cpvd_pesone[il_fila] = 0  Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rPeso Neto"
	ls_colu[li_cont] 	= "cpvd_pesone"
End If

If IsNull(dw_2.object.cpvd_resemb[il_fila]) OR dw_2.object.cpvd_resemb[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Embalaje"
	ls_colu[li_cont] 	= "cpvd_resemb"
End If

If IsNull(dw_2.object.cpvd_numfru[il_fila]) OR dw_2.object.cpvd_numfru[il_fila] = 0 Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNúmero de Frutos"
	ls_colu[li_cont] 	= "cpvd_numfru"
End If

If IsNull(dw_3.object.cpvd_rescal[il_fila]) OR dw_3.object.cpvd_rescal[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Calidad"
	ls_colu[li_cont] 	= "cpvd_rescal"
End If

If IsNull(dw_4.object.cpvd_rescon[il_fila]) OR dw_4.object.cpvd_rescon[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Condición"
	ls_colu[li_cont] 	= "cpvd_rescon"
End If

//
IF dw_5.object.cpvd_rescaj[il_fila] = 'O' THEN	
	IF Not IsNull(dw_5.object.cpvd_cobjem[il_fila]) OR dw_5.object.cpvd_cobjem[il_fila] > 0 THEN
		IF IsNull(dw_5.Object.cpvd_porob1[il_fila]) OR dw_5.Object.cpvd_porob1[il_fila] = 0 THEN
			li_cont ++
			ls_mensaje		 	= ls_mensaje + "~rResolución Caja Porcentaje de Causal 1"
			ls_colu[li_cont] 	= "cpvd_porob1"
		END IF	
	ELSE
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rResolución Caja Código Causal 1"
		ls_colu[li_cont] 	= "cpvd_rescaj"
	END IF
	
	IF Not IsNull(dw_5.object.cpvd_cobjca[il_fila]) OR dw_5.object.cpvd_cobjca[il_fila] > 0 THEN
		IF IsNull(dw_5.Object.cpvd_porob2[il_fila]) OR dw_5.Object.cpvd_porob2[il_fila] = 0 THEN
			li_cont ++
			ls_mensaje		 	= ls_mensaje + "~rResolución Caja Porcentaje de Causal 2"
			ls_colu[li_cont] 	= "cpvd_porob2"
		END IF	
	END IF
		
	IF Not IsNull(dw_5.object.cpvd_cobjco[il_fila] ) OR dw_5.object.cpvd_cobjco[il_fila] > 0 THEN
		IF IsNull(dw_5.Object.cpvd_porob3[il_fila]) OR dw_5.Object.cpvd_porob3[il_fila] = 0 THEN
			li_cont ++
			ls_mensaje		 	= ls_mensaje + "~rResolución Caja Porcentaje de Causal 3"
			ls_colu[li_cont] 	= "cpvd_porob3"
		END IF	
	END IF
	
END IF

	
IF dw_5.object.cpvd_respal[il_fila] = 'R' THEN	
	IF Not IsNull(dw_5.object.cpvd_causal1[il_fila]) Or dw_5.object.cpvd_causal1[il_fila] > 0  THEN
		IF IsNull(dw_5.Object.cpvd_pocau1[il_fila]) OR dw_5.Object.cpvd_pocau1[il_fila] = 0 THEN
			li_cont ++
			ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Porcentaje de Causal 1"
			ls_colu[li_cont] 	= "cpvd_pocau1"
		END IF	
	ELSE
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Código Causal 1"
		ls_colu[li_cont] 	= "cpvd_causal1"
	END IF
	
	IF dw_5.object.cpvd_causal2[il_fila] > 0 THEN
		IF IsNull(dw_5.Object.cpvd_pocau2[il_fila]) OR dw_5.Object.cpvd_pocau2[il_fila] = 0 THEN
			li_cont ++
			ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Porcentaje de Causal 2"
			ls_colu[li_cont] 	= "cpvd_pocau2"
		END IF	
	END IF

	IF dw_5.object.cpvd_causal3[il_fila] > 0 THEN
		IF IsNull(dw_5.Object.cpvd_pocau3[il_fila]) OR dw_5.Object.cpvd_pocau3[il_fila] = 0 THEN
			li_cont ++
			ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Porcentaje de Causal 3"
			ls_colu[li_cont] 	= "cpvd_pocau3"
		END IF	
	END IF
END IF
//


If IsNull(dw_5.object.cpvd_rescaj[il_fila]) OR dw_5.object.cpvd_rescaj[il_fila] = '' Then
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Caja"
	ls_colu[li_cont] 	= "cpvd_rescaj"
End If

//If dw_2.object.cpvd_resemb[il_fila] = 'M' Then
//	If IsNull(dw_2.object.cpvd_cobjem[il_fila]) OR dw_2.object.cpvd_cobjem[il_fila] = 0 Then
//		li_cont ++
//		ls_mensaje		 	= ls_mensaje + "~rCausal Objeción Embalaje"
//		ls_colu[li_cont] 	= "cpvd_cobjem"
//	End If
//End If
//
//If dw_3.object.cpvd_rescal[il_fila] = 'M' Then
//	If IsNull(dw_3.object.cpvd_cobjca[il_fila]) OR dw_3.object.cpvd_cobjca[il_fila] = 0 Then
//		li_cont ++
//		ls_mensaje		 	= ls_mensaje + "~rCausal Objeción Calidad"
//		ls_colu[li_cont] 	= "cpvd_cobjca"
//	End If
//End If
//
//If dw_4.object.cpvd_rescon[il_fila] = 'M' Then
//	If IsNull(dw_4.object.cpvd_cobjco[il_fila]) OR dw_4.object.cpvd_cobjco[il_fila] = 0 Then
//		li_cont ++
//		ls_mensaje		 	= ls_mensaje + "~rCausal Objeción Condición"
//		ls_colu[li_cont] 	= "cpvd_cobjco"
//	End If
//End If
//
If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_4.SetColumn(ls_colu[1]) 
	dw_4.SetFocus()
	Message.DoubleParm = -1
ELSEIf Round(dw_5.Object.cpvd_portot[il_fila],1) > 999.9 Then
	MessageBox("Error de Consistencia", "% Total Defectos no puede ser > a 999 :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_5.SetColumn("cpvd_portot") 
	dw_5.SetFocus()
	Message.DoubleParm = -1
ELSE
	Round(dw_5.Object.cpvd_portot[il_fila],1) 
End If
end event

event ue_nuevo;String	ls_rut, ls_Nula

SetNull(ls_Nula)

ib_ok = True

This.TriggerEvent("ue_guardar")

IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

wf_nuevo()

tab_1.SelectTab(1)

dw_1.SetFocus()
dw_1.SetItem(il_fila,"clie_codigo",Integer(istr_mant.Argumento[7]))
dw_1.SetItem(il_fila,"plde_codigo",Integer(istr_mant.Argumento[4]))

dw_2.SetRow(il_fila)
dw_3.SetRow(il_fila)
dw_4.SetRow(il_fila)
dw_5.SetRow(il_fila)
dw_6.SetRow(il_fila)
dw_7.SetRow(il_fila)

dw_2.ScrollToRow(il_fila)
dw_3.ScrollToRow(il_fila)
dw_4.ScrollToRow(il_fila)
dw_5.ScrollToRow(il_fila)
dw_6.ScrollToRow(il_fila)
dw_7.ScrollToRow(il_fila)

dw_2.SetItem(il_fila,"cpvd_paleti",ls_Nula)
dw_2.SetItem(il_fila,"cpvd_rotula",ls_Nula)
dw_2.SetItem(il_fila,"cpvd_materi",ls_Nula)
dw_2.SetItem(il_fila,"cpvd_empaq",ls_Nula)

dw_5.SetItem(il_fila,"cpvd_resemb",ls_Nula)
dw_5.SetItem(il_fila,"cpvd_rescal",ls_Nula)
dw_5.SetItem(il_fila,"cpvd_rescon",ls_Nula)
dw_5.SetItem(il_fila,"cpvd_rescaj",ls_Nula)
end event

event open;Long		ll_trans, ll_planilla, ll_guia
Integer	li_planta, li_cliente, li_zona, li_especie

This.Icon	=	Gstr_apl.Icono

Postevent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

x	= 100
y	= 450

SetPointer(HourGlass!)

li_planta	=	Integer(istr_mant.argumento[4])
li_cliente	=	Integer(istr_mant.argumento[7])
li_zona	=	Integer(istr_mant.argumento[3])
li_especie=	Integer(istr_mant.argumento[9])
ll_planilla	=	Long(istr_mant.argumento[2])
ll_guia	=	Long(istr_mant.argumento[13])

dw_1.Object.t_titulo.text = "Digitación Planilla Verificación de Limas"

dw_1.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1)
dw_1.SetItem(il_fila, "plde_codigo",li_planta)

dw_1.SetItem(il_fila, "ccpv_numero",ll_planilla)

dw_2	=	tab_1.tabpage_1.dw_embalaje
dw_3	=	tab_1.tabpage_2.dw_calidad
dw_4	=	tab_1.tabpage_3.dw_condicion
dw_5	=	tab_1.tabpage_4.dw_resolucion
dw_6	=	tab_1.tabpage_5.dw_id

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

istr_mant.dw.ShareData(dw_1)

dw_1.ShareData(dw_2)
dw_1.ShareData(dw_3)
dw_1.ShareData(dw_4)
dw_1.ShareData(dw_5)
dw_1.ShareData(dw_6)
dw_1.ShareData(dw_7)

//dw_5.Object.cpvd_rescom.protect = 0
//dw_2.Object.cpvd_cobjem.protect = 1
//dw_3.Object.cpvd_cobjca.protect = 1
//dw_4.Object.cpvd_cobjco.protect = 1
//dw_2.object.cpvd_cobjem.Background.color = RGB(192, 192, 192)
//dw_3.object.cpvd_cobjca.Background.color = RGB(192, 192, 192)
//dw_4.object.cpvd_cobjco.Background.color = RGB(192, 192, 192)

dw_7.SetFocus()
end event

event resize;//
end event

event ue_guardar;SetPointer(HourGlass!)

Message.DoubleParm = 0

IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_2.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_3.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_4.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_5.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_6.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN 
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcalplanillalimas
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcalplanillalimas
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcalplanillalimas
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcalplanillalimas
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcalplanillalimas
integer x = 3008
integer y = 344
integer taborder = 40
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcalplanillalimas
integer x = 3003
integer y = 156
integer taborder = 30
end type

event pb_acepta::clicked;String	ls_colu[],ls_mensaje
Integer	li_cont
istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE 
	IF Isnull(dw_1.object.cpvd_numpal[il_fila]) or dw_1.object.cpvd_numpal[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rNúmero de Pallet"
		ls_colu[li_cont] 	= "cpvd_numpal"
	END IF
	IF Isnull(dw_5.object.cpvd_segpal[il_fila]) or dw_5.object.cpvd_segpal[il_fila] = '' THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rSegregación Pallet"
		ls_colu[li_cont] 	= "cpvd_segpal"
	END IF
	IF Isnull(dw_5.object.cpvd_respal[il_fila]) or dw_5.object.cpvd_respal[il_fila] = '' THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rResolución Pallet"
		ls_colu[li_cont] 	= "cpvd_respal"
	END IF
	
	//
			IF dw_5.object.cpvd_rescaj[il_fila] = 'O' THEN	
				IF Not IsNull(dw_5.object.cpvd_cobjem[il_fila]) OR dw_5.object.cpvd_cobjem[il_fila] > 0 THEN
					IF IsNull(dw_5.Object.cpvd_porob1[il_fila]) OR dw_5.Object.cpvd_porob1[il_fila] = 0 THEN
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Caja Porcentaje de Causal 1"
						ls_colu[li_cont] 	= "cpvd_porob1"						
					END IF
				ELSE
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Caja Código Causal 1"
						ls_colu[li_cont] 	= "cpvd_cobjem"						
				END IF
			END IF
		
			IF Not IsNull(dw_5.object.cpvd_cobjca[il_fila]) OR dw_5.object.cpvd_cobjca[il_fila] > 0 THEN
				IF IsNull(dw_5.Object.cpvd_porob2[il_fila]) OR dw_5.Object.cpvd_porob2[il_fila] = 0 THEN
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Caja Porcentaje de Causal 2"
						ls_colu[li_cont] 	= "cpvd_porob2"											
				END IF	
			END IF
		
			IF Not IsNull(dw_5.object.cpvd_cobjco[il_fila] ) OR dw_5.object.cpvd_cobjco[il_fila] > 0 THEN
				IF IsNull(dw_5.Object.cpvd_porob3[il_fila]) OR dw_5.Object.cpvd_porob3[il_fila] = 0 THEN
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Caja Porcentaje de Causal 3"
						ls_colu[li_cont] 	= "cpvd_porob3"																
				END IF	
			END IF
		
			IF dw_5.object.cpvd_respal[il_fila] = 'R' THEN		
				IF dw_5.object.cpvd_causal1[il_fila] > 0 OR Not IsNull(dw_5.object.cpvd_causal1[il_fila]) THEN
					IF IsNull(dw_5.Object.cpvd_pocau1[il_fila]) OR dw_5.Object.cpvd_pocau1[il_fila] = 0 THEN
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Porcentaje de Causal 1"
						ls_colu[li_cont] 	= "cpvd_pocau1"																						
					END IF	
				ELSE
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Código Causal 1"
						ls_colu[li_cont] 	= "cpvd_causal1"																											
				END IF
			END IF
		
			
			IF dw_5.object.cpvd_causal2[il_fila] > 0 THEN
				IF isnull(dw_5.Object.cpvd_pocau2[il_fila]) OR dw_5.Object.cpvd_pocau2[il_fila] = 0 THEN
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Porcentaje de Causal 2"
						ls_colu[li_cont] 	= "cpvd_pocau2"																																
				END IF	
			END IF
			
			IF dw_5.object.cpvd_causal3[il_fila] > 0 THEN
				IF isnull(dw_5.Object.cpvd_pocau3[il_fila]) OR dw_5.Object.cpvd_pocau3[il_fila] = 0 THEN
						li_cont ++
						ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Porcentaje de Causal 3"
						ls_colu[li_cont] 	= "cpvd_pocau3"																																
				END IF	
			END IF
	
	//
	
//	IF dw_5.object.cpvd_cobjem[il_fila] > 0 THEN
//		IF isnull(dw_5.Object.cpvd_porob1[il_fila]) OR dw_5.Object.cpvd_porob1[il_fila] = 0 THEN
//			MessageBox("Error de Consistencia", "Falta el ingreso de Porcentaje de Causal 1.", StopSign!, Ok!)
//			Return
//		END IF	
//	END IF
//	
//	IF dw_5.object.cpvd_cobjca[il_fila] > 0 THEN
//		IF isnull(dw_5.Object.cpvd_porob2[il_fila]) OR dw_5.Object.cpvd_porob2[il_fila] = 0 THEN
//			MessageBox("Error de Consistencia", "Falta el ingreso de Porcentaje de Causal 2.", StopSign!, Ok!)
//			Return
//		END IF	
//	END IF
//	
//	IF dw_5.object.cpvd_cobjco[il_fila] > 0 THEN
//		IF isnull(dw_5.Object.cpvd_porob3[il_fila]) OR dw_5.Object.cpvd_porob3[il_fila] = 0 THEN
//			MessageBox("Error de Consistencia", "Falta el ingreso de Porcentaje de Causal 3.", StopSign!, Ok!)
//			Return
//		END IF	
//	END IF
		
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_5.SetColumn(ls_colu[li_cont]) 
		dw_5.SetFocus()
		Return
	END IF
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcalplanillalimas
integer x = 3003
integer y = 532
integer taborder = 50
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcalplanillalimas
integer y = 116
integer width = 2811
integer height = 628
string dataobject = "dw_mant_ctlcalplanillanaranjas"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_Nula, li_ccajas

SetNull(ls_Nula)
SetNull(li_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
	CASE "cpvd_numpal"
		IF not isnull(data) and data <> '' and gstr_parlote.codgen <> 1 THEN
			//This.Object.paen_ccajas.Protect = 1
			li_ccajas = ExistePallet(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[4]),Long(istr_mant.argumento[1]),long(data))
			
			IF isnull(li_ccajas) THEN
				
				This.SetItem(Row, ls_Columna, li_Nula)
				
				dw_5.setcolumn("cpvd_numpal")
				dw_5.setfocus()
				RETURN 1
			ELSE	
			
				dw_1.Object.paen_ccajas[il_fila] = li_ccajas
				
				IF il_fila > 1 THEN
					IF dw_1.Object.cpvd_numpal[il_Fila - 1] = Long(data) THEN
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_rescaj[il_fila]	=	dw_1.Object.cpvd_rescaj[il_fila - 1]
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_rescom[il_fila]	=	dw_1.Object.cpvd_rescom[il_fila - 1]
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_segpal[il_fila]	=	dw_1.Object.cpvd_segpal[il_fila - 1]
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_respal[il_fila]	=	dw_1.Object.cpvd_respal[il_fila - 1]
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_causal1[il_fila]	=	dw_1.Object.cpvd_causal1[il_fila - 1]
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_causal2[il_fila]	=	dw_1.Object.cpvd_causal2[il_fila - 1]
						tab_1.tabpage_4.dw_resolucion.Object.cpvd_causal3[il_fila]	=	dw_1.Object.cpvd_causal3[il_fila - 1]
					END IF
				END IF
				
			END IF	
		ELSE
			This.Object.paen_ccajas.Protect = 0			
		END IF

END CHOOSE

end event

type tab_1 from tab within w_mant_deta_ctlcalplanillalimas
integer x = 50
integer y = 820
integer width = 3397
integer height = 1024
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long backcolor = 12639424
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.tabpage_5=create tabpage_5
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4,&
this.tabpage_5}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
destroy(this.tabpage_5)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3360
integer height = 896
long backcolor = 16711680
string text = "Embalaje"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_embalaje dw_embalaje
end type

on tabpage_1.create
this.dw_embalaje=create dw_embalaje
this.Control[]={this.dw_embalaje}
end on

on tabpage_1.destroy
destroy(this.dw_embalaje)
end on

type dw_embalaje from uo_dw within tabpage_1
integer x = 55
integer y = 32
integer width = 2926
integer height = 816
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillanaranjas_embalaje"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_ValoRetorno

SetNull(ls_Nula)
ls_Columna	=	dwo.Name

Choose Case ls_Columna			
	Case  "cpvd_paleti","cpvd_rotula","cpvd_materi","cpvd_empaq", "cpvd_aparie"
 		DatosEmbalaje(ls_columna,data)
		If ii_valor	=	2 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			SetNull(ii_valor)
			Return 1
		End If
		If data = 'M' Then
			This.SetItem(Row, "cpvd_resemb",'M')
			dw_2.Object.cpvd_cobjem.protect = 0
			dw_2.object.cpvd_cobjem.Background.color = RGB(255, 255, 255)
		End If

	Case "cpvd_pesone"			
		li_ValoRetorno =	ValidaPeso(ls_columna,data)
		If li_ValoRetorno	=	0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1
		End If	

	Case "cpvd_resemb"
		If DatosResolucion(ls_columna,data) = False Then
			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
			  Return 1
		Else
			If data <> 'M' Then
				If ValidaCalIficacion(ls_columna) Then	
					dw_2.Object.cpvd_cobjem.protect = 0
					dw_2.object.cpvd_cobjem.Background.color = RGB(255, 255, 255)
					This.SetItem(il_fila, ls_Columna, 'M')
					Return 1
				End If
			Else
				dw_2.Object.cpvd_cobjem.protect = 0
				dw_2.object.cpvd_cobjem.Background.color = RGB(255, 255, 255)
				dw_2.setcolumn("cpvd_cobjem")
				dw_2.setfocus()
			End If

			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)

			If ls_resolucioncaja = 'A' Then
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'A')				  
			ElseIf ls_resolucioncaja = 'O' Then
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'O')
			Else 
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0  
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'A')				
			End If
		End If

		Case "cpvd_rescal"
			If Not Existe_Causal(Integer(data)) Then
				dw_2.Setitem(row, "cpvd_cobjem",Integer(ls_Nula))				
				Return 1
			End If 

		Case "cpvd_cobjem"
			If Not valida_Causal(ls_columna,Integer(data)) Then
				dw_2.Setitem(row, "cpvd_cobjem",Integer(ls_Nula))				
				Return 1
			End If 

			If Not Existe_Causal(Integer(data)) Then
				dw_2.Setitem(row, "cpvd_cobjem",Integer(ls_Nula))				
				Return 1
			End If
End Choose
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

event itemerror;Return 1
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3360
integer height = 896
long backcolor = 16711680
string text = "Calidad"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_calidad dw_calidad
end type

on tabpage_2.create
this.dw_calidad=create dw_calidad
this.Control[]={this.dw_calidad}
end on

on tabpage_2.destroy
destroy(this.dw_calidad)
end on

type dw_calidad from uo_dw within tabpage_2
integer x = 55
integer y = 44
integer width = 2770
integer height = 772
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillalimas_calidad"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_Nula

SetNull(ls_Nula)
SetNull(li_Nula)

ls_Columna	=	dwo.Name

Choose Case ls_Columna			
	Case	"cpvd_deform"
		If Integer(Data)	>	100	Then
			MessageBox("Atención","Deformes no puede ser mayor a 100")
			dw_Calidad.SetITem(il_Fila,"cpvd_deform",li_Nula)
			dw_Calidad.SetColumn("cpvd_deform")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1	
		End If	

  Case	"cpvd_heridas"
		If Integer(Data)	>	100	Then
			MessageBox("Atención","Heridas Cicatrizadas no puede ser mayor a 100")
			dw_Calidad.SetITem(il_Fila,"cpvd_heridas",li_Nula)
			dw_Calidad.SetColumn("cpvd_heridas")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1	
		End If	
		
	Case	"cpvd_mandor"
		If Integer(Data)	>	100	Then
			MessageBox("Atención","Mancha de Agua no puede ser mayor a 100")
			dw_Calidad.SetITem(il_Fila,"cpvd_mandor",li_Nula)
			dw_Calidad.SetColumn("cpvd_mandor")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1	
		End If	

	Case	"cpvd_residu"
		If Integer(Data)	>	100	Then
			MessageBox("Atención","Residuos no puede ser mayor a 100")
			dw_Calidad.SetITem(il_Fila,"cpvd_residu",li_Nula)
			dw_Calidad.SetColumn("cpvd_residu")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1	
		End If
		
	Case "cpvd_mancha"
		If	Integer(Data)	>	100	Then
			MessageBox("Atención","Marca Hayward  no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("cpvd_mancha")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			Return 1	
		End If		
		
	Case "cpvd_russet"
		If	Integer(Data)	>	100	Then
			MessageBox("Atención","Russet no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("cpvd_russet")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			Return 1	
		End If

	Case "cpvd_rusosc"
		If	Integer(Data)	>	100	Then
			MessageBox("Atención","Russet no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn(ls_Columna)
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			Return 1	
		End If
		
	Case "cpvd_insect"
		If	Integer(Data)	>	100	Then
			MessageBox("Atención","Insectos no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("cpvd_insect")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			Return 1	
		End If
		
	Case "cpvd_matext"
		If	Integer(Data)	>	100	Then
			MessageBox("Atención","Materias Extrañas no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("cpvd_matext")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			Return 1	
		End If		
		
	Case "cpvd_otrcal"
		If	Integer(Data)	>	100	Then
			MessageBox("Atención","Otros no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("cpvd_otrcal")
			Return 1
		End If
		
		If Dec(Data) > 999.9 OR Dec(Data) < 0 Then
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			Return 1	
		End If	
		
	Case "cpvd_rescal"
      If DatosResolucion(ls_columna,data) = False Then
			  This.SetItem(il_fila, ls_Columna, ' ')
			  Return 1
		Else
			 
         If data='M' Then
       		dw_3.Object.cpvd_cobjca.protect = 0
				dw_3.object.cpvd_cobjca.Background.color = RGB(255, 255, 255)
				dw_3.SetItem(il_fila, "cpvd_cobjca", Long(ls_nula))
				dw_3.setcolumn("cpvd_cobjca")
				dw_3.setfocus()
			ElseIf data<>'M' Then
				dw_3.Object.cpvd_cobjca.protect = 1
				dw_3.object.cpvd_cobjca.Background.color = RGB(192, 192, 192)
				dw_3.SetItem(il_fila, "cpvd_cobjca", Long(ls_nula))
				
			End If
			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
			If ls_resolucioncaja = 'A' Then
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'A')				  
		
			ElseIf ls_resolucioncaja = 'O' Then
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'O')

			Else 
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0  
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'A')

			End If
		End If				
			
	Case "cpvd_cobjca"
		If NOT valida_Causal(ls_columna,Integer(data)) Then
				dw_3.Setitem(row, "cpvd_cobjca",Integer(ls_Nula))				
				Return 1
			End If 
		
		If NOT existe_causal(Integer(data)) Then
			dw_2.Setitem(row, "cpvd_cobjca",integer(ls_Nula))				
			Return 1 
		End If 
End Choose

end event

event itemerror;RETURN 1 
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;ib_modifica = True
ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3360
integer height = 896
long backcolor = 16711680
string text = "Condición"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_condicion dw_condicion
end type

on tabpage_3.create
this.dw_condicion=create dw_condicion
this.Control[]={this.dw_condicion}
end on

on tabpage_3.destroy
destroy(this.dw_condicion)
end on

type dw_condicion from uo_dw within tabpage_3
integer x = 165
integer y = 32
integer width = 2203
integer height = 740
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillalimas_condicion"
boolean vscrollbar = false
end type

event itemchanged;ib_modifica = True

Integer	li_Nula, li_punuba, li_pununi, li_punura
String	ls_Nula, ls_Columna, ls_resolucioncaja

SetNull(li_Nula)
SetNull(ls_Nula)

ls_Columna	=	dwo.name

CHOOSE CASE ls_Columna
	
	CASE "cpvd_firpul"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
	CASE "cpvd_firpu2"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF	
	
	CASE "cpvd_herabi"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		

	CASE "cpvd_frubla"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
	
	CASE "cpvd_pudric"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF

	CASE "cpvd_deship"	
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF			
	
	CASE "cpvd_machuc"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
			
	CASE "cpvd_otrcon"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
	CASE	"cpvd_golsol"
		
		IF Integer(Data)	>	100	THEN
			MessageBox("Atención","Golpe/Quemado de Sol no puede ser mayor a 100")
			dw_Condicion.SetITem(il_Fila,"cpvd_golsol",li_Nula)
			dw_Condicion.SetColumn("cpvd_golsol")
			RETURN 1
		END IF
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1	
		END IF			
		
	CASE "cpvd_rescon"
		IF DatosResolucion(ls_columna,data) = False THEN
			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
			  RETURN 1
		ELSE
			IF data<>'M' THEN
				dw_4.Object.cpvd_cobjco.protect = 1
				dw_4.object.cpvd_cobjco.Background.color = RGB(192, 192, 192)
				dw_4.SetItem(il_fila, "cpvd_cobjco", Long(ls_nula))
				dw_4.setfocus()
			ELSE
				dw_4.Object.cpvd_cobjco.protect = 0
				dw_4.object.cpvd_cobjco.Background.color = RGB(255, 255, 255)
				dw_4.SetItem(il_fila, "cpvd_cobjco", Long(ls_nula))
				dw_4.setcolumn("cpvd_cobjco")
				dw_4.setfocus()
			END IF
			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
			IF ls_resolucioncaja = 'A' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'A')				  
			
			ELSEIF ls_resolucioncaja = 'O' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'O')
			ELSE 
				tab_1.Tabpage_4.dw_resolucion.Object.cpvd_rescaj.protect = 0  
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "cpvd_rescaj", 'A')

			END IF
		END IF
		
	CASE "cpvd_cobjco"
		IF NOT valida_Causal(ls_columna,Integer(data)) THEN
				dw_4.Setitem(row, "cpvd_cobjco",Integer(ls_Nula))				
				RETURN 1
			END IF 
		
		IF NOT existe_causal(Integer(data)) THEN
			dw_2.Setitem(row, "cpvd_cobjco",integer(ls_Nula))				
			RETURN 1 
		END IF 
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3360
integer height = 896
long backcolor = 16711680
string text = "Resolución"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_resolucion dw_resolucion
end type

on tabpage_4.create
this.dw_resolucion=create dw_resolucion
this.Control[]={this.dw_resolucion}
end on

on tabpage_4.destroy
destroy(this.dw_resolucion)
end on

type dw_resolucion from uo_dw within tabpage_4
integer x = 229
integer y = 32
integer width = 1605
integer height = 760
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillalimas_resol"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_Nula

SetNull(ls_Nula)
SetNull(li_Nula)

Tab_1.TabPage_4.dw_resolucion.AcceptText()

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			

  CASE "cpvd_rescaj"
		IF data = 'A' THEN	
			This.SetItem(il_fila, "cpvd_rescom", 'A')
			This.SetItem(il_fila, "cpvd_respal", 'A')		
			This.Object.cpvd_rescom.protect = 1
			This.Object.cpvd_respal.protect = 1
			This.SetItem(il_fila, "cpvd_cobjem", li_Nula)
			This.SetItem(il_fila, "cpvd_cobjca", li_Nula)
			This.SetItem(il_fila, "cpvd_cobjco", li_Nula)	
			This.SetItem(il_fila, "cpvd_porob1", li_Nula)
			This.SetItem(il_fila, "cpvd_porob2", li_Nula)
			This.SetItem(il_fila, "cpvd_porob3", li_Nula)	
			This.SetItem(il_fila, "cpvd_causal1", li_Nula)
			This.SetItem(il_fila, "cpvd_causal2", li_Nula)
			This.SetItem(il_fila, "cpvd_causal3", li_Nula)	
			This.SetItem(il_fila, "cpvd_pocau1", li_Nula)
			This.SetItem(il_fila, "cpvd_pocau2", li_Nula)
			This.SetItem(il_fila, "cpvd_pocau3", li_Nula)	

		ELSEIF data = 'O' THEN	
			This.SetItem(il_fila, "cpvd_rescom", 'O')
			This.SetItem(il_fila, "cpvd_respal", 'R')
			This.SetItem(il_fila, "cpvd_causal1", li_Nula)
			This.SetItem(il_fila, "cpvd_causal2", li_Nula)
			This.SetItem(il_fila, "cpvd_causal3", li_Nula)		
			This.Object.cpvd_rescom.protect = 0
			This.Object.cpvd_respal.protect = 0
//			This.SetItem(il_fila, "ccpd_rescom", 'A')	
		END IF		

  CASE "cpvd_rescom"
		IF data = 'A' THEN	
			This.SetItem(il_fila, "cpvd_respal", 'A')
			This.Object.cpvd_respal.protect = 1
			This.SetItem(il_fila, "cpvd_causal1", li_Nula)
			This.SetItem(il_fila, "cpvd_causal2", li_Nula)
			This.SetItem(il_fila, "cpvd_causal3", li_Nula)	
			This.SetItem(il_fila, "cpvd_pocau1", li_Nula)
			This.SetItem(il_fila, "cpvd_pocau2", li_Nula)
			This.SetItem(il_fila, "cpvd_pocau3", li_Nula)	
			
		ELSEIF data = 'O' THEN	
			This.SetItem(il_fila, "cpvd_respal", 'R')
			This.SetItem(il_fila, "cpvd_causal1", li_Nula)
			This.SetItem(il_fila, "cpvd_causal2", li_Nula)
			This.SetItem(il_fila, "cpvd_causal3", li_Nula)	
			This.Object.cpvd_respal.protect = 0
		END IF		

//  CASE "ccpd_cobjem"
//		IF NOT existe_causal(Integer(data)) OR valida_causal(ls_columna,Integer(Data)) = FALSE THEN
//			dw_2.Setitem(row, "ccpd_cobjem",integer(ls_Nula))				
//			RETURN 1
//		END IF 
			
  CASE "cpvd_respal"
		IF data = 'R' OR data = 'P' THEN	
			This.SetItem(il_fila, "cpvd_causal1", li_Nula)
			This.SetItem(il_fila, "cpvd_causal2", li_Nula)
			This.SetItem(il_fila, "cpvd_causal3", li_Nula)
		END IF		
		
	Case	"cpvd_causal1"	
		If NOT Causales(Integer(data),Integer(istr_mant.argumento[9])) Then
			This.SetItem(il_fila, "cpvd_causal1", li_Nula)
			RETURN 1
		End If		
		
	Case	"cpvd_causal2"	
		If NOT Causales(Integer(data),Integer(istr_mant.argumento[9])) Then
			This.SetItem(il_fila, "cpvd_causal2", li_Nula)
			RETURN 1
		End If		
		
	Case	"cpvd_causal3"	
		If NOT Causales(Integer(data),Integer(istr_mant.argumento[9])) Then
			This.SetItem(il_fila, "cpvd_causal3", li_Nula)
			RETURN 1			
		End If				

	Case	"cpvd_pocau1"
			If Integer(data) > 100.00 Then
				MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
				This.SetItem(il_fila, "cpvd_pocau1" , li_Nula)
				RETURN 1 
			End If		
		
	Case	"cpvd_pocau2"		
			If Integer(data) > 100.00 Then
				MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
				This.SetItem(il_fila, "cpvd_pocau2", li_Nula)
				RETURN 1 
			End If
	
	Case	"cpvd_pocau3"
		If Integer(data) > 100.00 Then
			MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
			This.SetItem(il_fila, "cpvd_pocau3", li_Nula)
			RETURN 1 
		End If	
		
	Case	"cpvd_cobjca"	
		IF Causales2(Integer(data),Integer(istr_mant.argumento[9])) Then
			This.SetItem(il_fila, "cpvd_cobjca", li_Nula)
			RETURN 1
		END IF
		
	Case	"cpvd_cobjco"	
		IF Causales2(Integer(data),Integer(istr_mant.argumento[9])) Then
			This.SetItem(il_fila, "cpvd_cobjco", li_Nula)
			RETURN 1
		END IF
		
	Case	"cpvd_cobjem"	
		IF Causales2(Integer(data),Integer(istr_mant.argumento[9])) Then
			This.SetItem(il_fila, "cpvd_cobjem", li_Nula)
			RETURN 1			
		END IF					
END CHOOSE
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

event itemerror;RETURN 1
end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

type tabpage_5 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 112
integer width = 3360
integer height = 896
long backcolor = 16711680
string text = "Identificación"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_id dw_id
end type

on tabpage_5.create
this.dw_id=create dw_id
this.Control[]={this.dw_id}
end on

on tabpage_5.destroy
destroy(this.dw_id)
end on

type dw_id from uo_dw within tabpage_5
boolean visible = false
integer x = 699
integer y = 248
integer width = 1627
integer height = 456
integer taborder = 21
string dataobject = "dw_mant_ctlcalplanillacerezas_id"
boolean vscrollbar = false
end type

event itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF


end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

type dw_7 from datawindow within w_mant_deta_ctlcalplanillalimas
boolean visible = false
integer x = 87
integer y = 1892
integer width = 1655
integer height = 392
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_ctlcalplanillacerezas_id"
borderstyle borderstyle = styleraised!
end type

