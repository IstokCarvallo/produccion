$PBExportHeader$w_mant_deta_planillasdestinodet.srw
forward
global type w_mant_deta_planillasdestinodet from w_mant_detalle_csd
end type
type tab_1 from tab within w_mant_deta_planillasdestinodet
end type
type tabpage_5 from userobject within tab_1
end type
type dw_id from uo_dw within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_id dw_id
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
type tab_1 from tab within w_mant_deta_planillasdestinodet
tabpage_5 tabpage_5
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type
end forward

global type w_mant_deta_planillasdestinodet from w_mant_detalle_csd
integer width = 3442
integer height = 2652
boolean controlmenu = true
tab_1 tab_1
end type
global w_mant_deta_planillasdestinodet w_mant_deta_planillasdestinodet

type variables
//Integer	ii_Punuba, ii_Punura, ii_Pununi
//String	is_Columna
//Integer ii_cliente, ii_plde,ii_cclo,ii_ccpe,ii_valor
Integer ii_valor

DataWindowChild idwc_clientes,idwc_productor,idwc_especie,idwc_variedad,idwc_etiqueta
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
public function boolean existe_matextraña (string data)
public subroutine buscaembalaje ()
public function boolean noexisteespecie (integer ai_cliente, integer ai_especie)
public function boolean noexisteembalaje (integer ai_cliente, string as_embalaje)
public function boolean noexistecalibre (integer ai_especie, integer ai_variedad, string as_calibre)
public function boolean noexisteetiqueta (integer ai_etiqueta)
public function boolean noexisteproductor (integer ai_productor)
public function boolean noexistevariedad (integer ai_especie, integer ai_variedad)
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
RETURN TRUE
//
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
RETURN True
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
RETURN True
end function

public function boolean cienxciento (string columna, string valor);//Integer	li_valor1, li_valor2, li_valor3, li_valor4, li_valor5, li_valor6, li_suma
//String	ls_Null
//
//SetNull(ls_Null)
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_tamayo"
//		li_valor1	=	Integer(valor)
//		li_Suma		=	li_Valor1
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//		
//	CASE "ccpd_tamba1"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]
//		li_valor2	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//		
//	CASE "ccpd_tamba2"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]		
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_valor3	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//
//	CASE "ccpd_tamba3"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]		
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba2[il_Fila]		
//		li_valor4 	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3	+	li_Valor4
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//
//	CASE "ccpd_tamba4"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba2[il_Fila]		
//		li_Valor4	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba3[il_Fila]		
//		li_valor5 	=	Integer(valor)
//		li_Suma		=	li_Valor1 +	li_Valor2 +	li_Valor3 +	li_Valor4 +	li_Valor5
//		IF li_suma > 100 THEN	
//			MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//	
//	CASE "ccpd_tameno"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba2[il_Fila]		
//		li_Valor4	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba3[il_Fila]
//		li_valor5	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba4[il_Fila]
//		li_valor6 	=	Integer(valor)
//		li_Suma		=	li_Valor1 +	li_Valor2 +	li_Valor3 +	li_Valor4 +	li_Valor5 + li_valor6
//		IF li_suma > 100 OR li_Suma <	100	THEN			
//			MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//
//END CHOOSE
//
RETURN TRUE
//
end function

public subroutine datosembalaje (string columna, string valor);Integer	li_v4
String	ls_v1, ls_v2, ls_v3
Dec{1}	ld_v5


ls_v1				=	dw_2.Object.cpdd_detpal[il_fila]
ls_v2				=	dw_2.Object.cpdd_rotula[il_fila]
ls_v3				=	dw_2.Object.cpdd_matint[il_fila]
li_v4				=	dw_2.Object.cpdd_nropaq[il_fila]
ld_v5				=	dw_2.Object.cpdd_calemb[il_fila]


CHOOSE CASE Columna

	CASE "cpdd_detpal"
		ls_v1	=	valor
		IF ls_v1 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Paletizaje.", StopSign!, Ok!)					
			dw_2.SetFocus()
		END IF

	CASE "ccpd_rotula"
		ls_v2	=	valor
		IF ls_v2 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Rotulación.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "ccpd_materi"
		ls_v3	=	valor
		IF ls_v3 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Materiales.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "ccpd_nropaq"
		li_v4 =	Integer(valor)
		IF li_v4 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Número Paquetes.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "ccpd_calemb"
		ld_v5 =	Integer(valor)
		IF ld_v5 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Calificación de Embalaje.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

END CHOOSE

end subroutine

public function boolean datosresolucion (string columna, string valor);//String	ls_v1, ls_v2, ls_v3, ls_v4
//
//ls_v1		=	dw_5.Object.ccpd_resemb[il_fila]
//ls_v2		=	dw_5.Object.ccpd_rescal[il_fila]
//ls_v3		=	dw_5.Object.ccpd_rescon[il_fila]
//
//
//
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_resemb"
//		ls_v1	=	valor
//		IF ls_v1 = '' THEN
//			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Embalaje.", StopSign!, Ok!)
//			dw_5.SetFocus()
//			RETURN False
//		END IF
//
//	CASE "ccpd_rescal"
//		ls_v2	=	valor
//		IF ls_v2 = '' THEN
//			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Calidad.", StopSign!, Ok!)
//			dw_5.SetFocus()
//			RETURN False
//		END IF
//
//	CASE "ccpd_rescon"
//		ls_v3	=	valor
//		IF ls_v3 = '' THEN
//			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Condición.", StopSign!, Ok!)
//			dw_5.SetFocus()
//			RETURN False			
//		END IF
//		
//END CHOOSE
//
RETURN True
end function

public function string datosresolucioncaja (string columna, string valor);//String	ls_v1, ls_v2, ls_v3, ls_v4, ls_juntos
//
//ls_v1		=	dw_5.Object.ccpd_resemb[il_fila]
//ls_v2		=	dw_5.Object.ccpd_rescal[il_fila]
//ls_v3		=	dw_5.Object.ccpd_rescon[il_fila]
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_resemb"
//		ls_v1	=	valor
//
//	CASE "ccpd_rescal"
//		ls_v2	=	valor
//
//	CASE "ccpd_rescon"
//		ls_v3	=	valor
//		
//END CHOOSE
//
//ls_juntos	=	ls_v1+ls_v2+ls_v3
//
//IF ls_juntos = 'BBB' THEN 
//	ls_v4 = 'A'
//ELSEIF ls_juntos ='BBR' OR ls_juntos ='RBB' OR ls_juntos ='BRB' THEN
//	ls_v4 = 'A'
//ELSEIF ls_v1 = 'M' OR ls_v2 = 'M' OR ls_v3 = 'M' THEN
//	ls_v4 = 'O'
//ELSE 
//	ls_v4 = 'Z'
//END IF
//
//RETURN ls_v4
return 's'
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
	RETURN False
//END IF
//
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
//FROM	 dba.ctlcaldanoespecie
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

RETURN FALSE


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
RETURN True
end function

public function boolean existedañoinsecto (string data);//Integer	li_Daño	, li_Existe
//
//li_Daño	=	Integer(Data)
//
//SELECT Count(*)
//INTO	 :li_Existe
//FROM	 dba.ctlcaldanoespecie
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
RETURN FALSE
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

Return True




end function

public function boolean cienporciento (string columna, string valor);//Integer	li_valor1, li_valor2, li_valor3, li_valor4, li_suma
//String	ls_Null
////Dec{2}
//
//SetNull(ls_Null)
//
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_cobay1"
//		li_valor1	=	Integer(valor)
//		li_Suma		=	li_Valor1	
//		IF li_suma > 100	THEN
//				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
//				dw_3.SetFocus()
//				RETURN FALSE
//		END IF
//		
//
//	CASE "ccpd_cobay2"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay1[il_Fila]
//		li_valor2	=	Integer(valor)
//		
//		li_Suma		=	li_Valor1	+	li_Valor2
//		IF  li_suma > 100	THEN
//				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
//				dw_3.SetFocus()
//				RETURN FALSE
//		END IF
//		
//	
//	CASE "ccpd_cobay3"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay1[il_Fila]
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay2[il_Fila]
//		li_valor3	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3
//		IF  li_suma > 100	THEN
//				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
//				dw_3.SetFocus()
//				RETURN FALSE
//		END IF
//		
//		
//	CASE "ccpd_cobay4"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay1[il_Fila]
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay2[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay3[il_Fila]		
//		li_valor4 	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3	+	li_Valor4
//		IF  li_suma > 100 OR	li_Suma	<	100	THEN
//				MessageBox("Error de Consistencia", "La Suma de Color Bayas debe dar siempre 100.", StopSign!, Ok!)
//				dw_3.SetFocus()
//				RETURN FALSE
//		END IF
//		
//END CHOOSE
//
RETURN TRUE
//
//
//
end function

public function integer validapeso (string columna, string valor);//Dec{2}	ld_valor1, ld_PesoMax, ld_PesoMin
//String	ls_Null
//
//SetNull(ls_Null)
//
//ld_valor1	=	Dec(valor)
//		
//SELECT	emba_pesmax, emba_pesmin
//	INTO 	:ld_PesoMax, :ld_PesoMin  
// 	FROM 	dba.embalajes
//	WHERE emba_codigo = :istr_mant.argumento[17]
//	AND	clie_codigo = 81 ;
//	
//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Embalajes")
//ELSEIF	ld_PesoMax = 0 AND ld_PesoMin = 0	THEN	
//	MessageBox("Atención","Código Embalaje No tiene Ingrsado Peso Mínimo y/o Peso Máximo Existe")
//ELSE
//	 IF ld_valor1 > ld_PesoMax OR ld_valor1 < ld_PesoMin THEN
//		MessageBox("Error de Consistencia", " Peso fuera de rango.", StopSign!, Ok!)
//		dw_3.SetFocus()
//	 	 ld_valor1=0
//	 	 ii_valor =0
// 	 END IF
//END IF	
//
//RETURN ld_valor1
return 1
//
end function

public function boolean existe_matextraña (string data);//Integer li_matext, li_existe
//
//li_matext	=	integer(data)
//
//SELECT Count(*)
//INTO	 :li_Existe
//FROM	 dba.ctlcaldanoespecie
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
RETURN FALSE
//
//
//
//
//
end function

public subroutine buscaembalaje ();istr_busq.argum[1] = String(gi_CodExport)

OpenWithParm(w_busc_embalajes, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_2.Object.emba_codigo[1]  = istr_busq.Argum[2]
END IF
end subroutine

public function boolean noexisteespecie (integer ai_cliente, integer ai_especie);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.especies
	WHERE espe_codigo = :ai_especie
	AND   clie_codigo = :ai_cliente;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Especies")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Especie No Existe Para Cliente" + &
					"~n~nSeleccionado, Ingrese Otra Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean noexisteembalaje (integer ai_cliente, string as_embalaje);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.embalajesprod
	WHERE clie_codigo = :ai_cliente
	AND   emba_codigo = :as_embalaje;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajes")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Embalaje No Existe Para Cliente" + &
					"~n~nSeleccionado, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean noexistecalibre (integer ai_especie, integer ai_variedad, string as_calibre);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.variecalibre
	WHERE vaca_calibr = :as_calibre
	AND   vari_codigo = :ai_variedad
	AND   espe_codigo = :ai_especie;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Calibre No Existe Para Especie, Variedad" + &
					"~n~nSeleccionadas, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean noexisteetiqueta (integer ai_etiqueta);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.etiquetas
	WHERE etiq_codigo = :ai_etiqueta;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Etiquetas")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Etiqueta No Existe Para Cliente" + &
					"~n~nSeleccionado, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean noexisteproductor (integer ai_productor);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.productores
	WHERE prod_codigo = :ai_productor;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Productor No Existe Para Cliente" + &
					"~n~nSeleccionado, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

	
end function

public function boolean noexistevariedad (integer ai_especie, integer ai_variedad);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.variedades
	WHERE vari_codigo = :ai_variedad
	AND   espe_codigo = :ai_especie;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Variedades")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Variedad No Existe Para Especie y Cliente" + &
					"~n~nSeleccionado, Ingrese Otra Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

on w_mant_deta_planillasdestinodet.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_mant_deta_planillasdestinodet.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;IF istr_mant.Agrega THEN
	dw_1.SetItem(il_Fila,"clie_codigo",Integer(istr_mant.Argumento[6]))
	dw_1.SetItem(il_Fila,"cpde_numero",Integer(istr_mant.Argumento[1]))
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
	pb_acepta.Enabled		= True
	pb_cancela.Enabled	= True
END IF 	
	
	 

end event

event ue_deshace;call super::ue_deshace;IF  dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN
	IF UpperBound(ias_campo) > 0 THEN
		dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[1]))
		dw_1.SetItem(il_fila, "cpde_numero", long(ias_campo[2]))
		dw_1.SetItem(il_fila, "cpdd_secuen", long(ias_campo[3]))
		dw_1.SetItem(il_fila, "cpde_fecins", date(ias_campo[4]))
		dw_1.SetItem(il_fila, "prod_codigo", Integer(ias_campo[5]))
		dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[6]))
		dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[7]))
		dw_1.SetItem(il_fila, "emba_codigo", ias_campo[8])
		dw_1.SetItem(il_fila, "cpdd_tamlot", Integer(ias_campo[9]))
		dw_1.SetItem(il_fila, "cpdd_nropal", Long(ias_campo[10]))
		dw_1.SetItem(il_fila, "cpdd_temper", Integer(ias_campo[11]))
		dw_1.SetItem(il_fila, "cpdd_calibr", ias_campo[12])
		dw_1.SetItem(il_fila, "etiq_codigo", Integer(ias_campo[13]))
		dw_1.SetItem(il_fila, "cpdd_fecemb", Date(ias_campo[14]))
		dw_1.SetItem(il_fila, "cpdd_detpal", ias_campo[15])
		dw_1.SetItem(il_fila, "cpdd_rotula", ias_campo[16])
		dw_1.SetItem(il_fila, "cpdd_matint", ias_campo[17])
		dw_1.SetItem(il_fila, "cpdd_nropaq", Integer(ias_campo[18]))
		dw_1.SetItem(il_fila, "cpdd_calemb", Integer(ias_campo[19]))
		dw_1.SetItem(il_fila, "cpdd_cbsoco", Integer(ias_campo[20]))
		dw_1.SetItem(il_fila, "cpdd_cbmayo", Integer(ias_campo[21]))
		dw_1.SetItem(il_fila, "cpdd_cbbaya", Integer(ias_campo[22]))
		dw_1.SetItem(il_fila, "cpdd_cbmeno", Integer(ias_campo[23]))
		dw_1.SetItem(il_fila, "cpdd_tbayxl", Integer(ias_campo[24]))
		dw_1.SetItem(il_fila, "cpdd_tbayae", Integer(ias_campo[25]))
		dw_1.SetItem(il_fila, "cpdd_tbayar", Integer(ias_campo[26]))
		dw_1.SetItem(il_fila, "cpdd_tba300", Integer(ias_campo[27]))
		dw_1.SetItem(il_fila, "cpdd_tbmeno", Integer(ias_campo[28]))
		dw_1.SetItem(il_fila, "cpdd_racdef", Integer(ias_campo[29]))
		dw_1.SetItem(il_fila, "cpdd_racapr", Integer(ias_campo[30]))
		dw_1.SetItem(il_fila, "cpdd_rasbpe", Integer(ias_campo[31]))
		dw_1.SetItem(il_fila, "cpdd_rapeba", Integer(ias_campo[32]))
		dw_1.SetItem(il_fila, "cpdd_defman", Integer(ias_campo[33]))
		dw_1.SetItem(il_fila, "cpdd_defgso", Integer(ias_campo[34]))
		dw_1.SetItem(il_fila, "cpdd_defres", Integer(ias_campo[35]))
		dw_1.SetItem(il_fila, "cpdd_grbrix", Integer(ias_campo[36]))
		dw_1.SetItem(il_fila, "cpdd_calcal", Integer(ias_campo[37]))
		dw_1.SetItem(il_fila, "cpdd_deslev", Integer(ias_campo[38]))
		dw_1.SetItem(il_fila, "cpdd_desmod", Integer(ias_campo[39]))
		dw_1.SetItem(il_fila, "cpdd_desalt", Integer(ias_campo[40]))
		dw_1.SetItem(il_fila, "cpdd_pudbay", Integer(ias_campo[41]))
		dw_1.SetItem(il_fila, "cpdd_pudnid", Integer(ias_campo[42]))
		dw_1.SetItem(il_fila, "cpdd_pudrac", Integer(ias_campo[43]))
		dw_1.SetItem(il_fila, "cpdd_conden", Integer(ias_campo[44]))
		dw_1.SetItem(il_fila, "cpdd_pardea", Integer(ias_campo[45]))
		dw_1.SetItem(il_fila, "cpdd_danso2", Integer(ias_campo[46]))
		dw_1.SetItem(il_fila, "cpdd_machuc", Integer(ias_campo[47]))
		dw_1.SetItem(il_fila, "cpdd_aplast", Integer(ias_campo[48]))
		dw_1.SetItem(il_fila, "cpdd_traslu", Integer(ias_campo[49]))
		dw_1.SetItem(il_fila, "cpdd_watber", Integer(ias_campo[50]))
		dw_1.SetItem(il_fila, "cpdd_parhum", Integer(ias_campo[51]))
		dw_1.SetItem(il_fila, "cpdd_paseco", Integer(ias_campo[52]))
		dw_1.SetItem(il_fila, "cpdd_desped", Integer(ias_campo[53]))
		dw_1.SetItem(il_fila, "cpdd_frubla", Integer(ias_campo[54]))
		dw_1.SetItem(il_fila, "cpdd_desgra", Integer(ias_campo[55]))	
		dw_1.SetItem(il_fila, "cpdd_calcon", Integer(ias_campo[56]))
		dw_1.SetItem(il_fila, "cpdd_guacaj", Integer(ias_campo[57]))
		dw_1.SetItem(il_fila, "cpdd_gualot", Integer(ias_campo[58]))
		dw_1.SetItem(il_fila, "cpdd_nfotos", Integer(ias_campo[59]))
		dw_1.SetItem(il_fila, "cpdd_observ", Integer(ias_campo[60]))
		
	END IF
END IF
end event

event ue_antesguardar;Integer	li_cont, li_suma, li_Nula, li_punuba, li_pununi, li_punura
String	ls_mensaje, ls_colu[]

SetNull(li_Nula)

IF Isnull(dw_6.object.cpdd_nropal[il_fila]) or dw_6.object.cpdd_nropal[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rPaletizaje"
	ls_colu[li_cont] 	= "cpdd_nropal"
END IF
IF Isnull(dw_6.object.cpdd_fecemb[il_fila])  THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rFecha de Embalaje"
	ls_colu[li_cont] 	= "cpdd_fecemb"
END IF
IF Isnull(dw_6.object.prod_codigo[il_fila]) or dw_6.object.prod_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rProductor"
	ls_colu[li_cont] 	= "prod_codigo"
END IF
IF Isnull(dw_6.object.espe_codigo[il_fila]) or dw_6.object.espe_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rEspecie"
	ls_colu[li_cont] 	= "espe_codigo"
END IF
IF Isnull(dw_6.object.vari_codigo[il_fila]) or dw_6.object.vari_codigo[il_fila] = 0  THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rVariedad"
	ls_colu[li_cont] 	= "vari_codigo"
END IF
IF Isnull(dw_6.object.cpdd_calibr[il_fila]) or dw_6.object.cpdd_calibr[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rCalibre"
	ls_colu[li_cont] 	= "cpdd_calibr"
END IF
IF Isnull(dw_6.object.emba_codigo[il_fila]) or dw_6.object.emba_codigo[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rEmbalaje"
	ls_colu[li_cont] 	= "emba_codigo"
END IF
IF Isnull(dw_6.object.etiq_codigo[il_fila]) or dw_6.object.etiq_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rCódigo Etiqueta"
	ls_colu[li_cont] 	= "etiq_codigo"
END IF


IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_6.SetColumn(ls_colu[1]) 
	dw_6.SetFocus()
	Message.DoubleParm = -1
END IF
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
dw_1.SetItem(il_fila,"clie_codigo",Integer(istr_mant.Argumento[6]))

dw_2.SetRow(il_fila)
dw_3.SetRow(il_fila)
dw_4.SetRow(il_fila)
dw_5.SetRow(il_fila)
dw_6.SetRow(il_fila)

dw_2.ScrollToRow(il_fila)
dw_3.ScrollToRow(il_fila)
dw_4.ScrollToRow(il_fila)
dw_5.ScrollToRow(il_fila)
dw_6.ScrollToRow(il_fila)


end event

event open;/*Argumentos
istr_mant.argumento[1] = planilla
istr_mant.argumento[2] = nave
istr_mant.argumento[3] = recibidor
istr_mant.argumento[4] = inspector
istr_mant.argumento[5] = puerto
istr_mant.argumento[6] = cliente
*/


Long		ll_trans, ll_planilla
Integer	li_cliente, li_especie, li_variedad, li_etiqueta

This.Icon	=	Gstr_apl.Icono

Postevent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

x	= 100
y	= 450

//x				= 0
//y				= 0
This.Height	= 2060


SetPointer(HourGlass!)

li_cliente	=	Integer(istr_mant.argumento[6])
ll_planilla	=	Long(istr_mant.argumento[1])

//Ciente
dw_1.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
idwc_clientes.Retrieve()

dw_1.SetItem(il_fila, "clie_codigo",li_cliente)
dw_1.SetItem(il_fila, "cpde_numero",ll_planilla)


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

istr_mant.dw.ShareData(dw_1)

dw_1.ShareData(dw_2)
dw_1.ShareData(dw_3)
dw_1.ShareData(dw_4)
dw_1.ShareData(dw_5)
dw_1.ShareData(dw_6)

//Productor
dw_6.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
IF idwc_productor.Retrieve() = 0 THEN
	idwc_productor.InsertRow(0)
END IF

//Especie
dw_6.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	idwc_especie.InsertRow(0)
END IF

//Variedad
dw_6.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
IF idwc_variedad.Retrieve(0) = 0 THEN
	idwc_variedad.InsertRow(0)
END IF

//Etiqueta
dw_6.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
IF idwc_etiqueta.Retrieve() = 0 THEN
	idwc_etiqueta.InsertRow(0)
END IF


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

event resize;//
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_planillasdestinodet
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_planillasdestinodet
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_planillasdestinodet
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_planillasdestinodet
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_planillasdestinodet
integer x = 2939
integer y = 344
integer taborder = 40
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_planillasdestinodet
integer x = 2935
integer y = 156
integer taborder = 30
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_planillasdestinodet
integer x = 2935
integer y = 564
integer taborder = 50
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_planillasdestinodet
integer x = 55
integer y = 168
integer width = 2565
integer height = 508
string dataobject = "dw_mant_deta_planillasdestinosdet"
end type

type tab_1 from tab within w_mant_deta_planillasdestinodet
integer x = 55
integer y = 812
integer width = 3113
integer height = 1108
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 12632256
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_5 tabpage_5
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type

on tab_1.create
this.tabpage_5=create tabpage_5
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.Control[]={this.tabpage_5,&
this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4}
end on

on tab_1.destroy
destroy(this.tabpage_5)
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
end on

type tabpage_5 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3077
integer height = 980
long backcolor = 12632256
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
integer x = 142
integer y = 92
integer width = 2807
integer height = 824
integer taborder = 21
string dataobject = "dw_mant_planillasdestinodet_id"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
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

event clicked;call super::clicked;CHOOSE CASE dwo.Name
	
	CASE "buscaembalaje"
		BuscaEmbalaje()
   	
END CHOOSE
end event

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula
SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
  
	 CASE "prod_codigo"
		IF NoExisteProductor(Integer(data)) THEN
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			RETURN 1
		END IF
		  
	  CASE "espe_codigo"
		  IF NoExisteEspecie(dw_1.Object.clie_codigo[1],Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSE
			dw_1.GetChild("vari_codigo", idwc_variedad)
         idwc_variedad.SetTransObject(sqlca)
         idwc_variedad.Retrieve(dw_1.Object.clie_codigo[1],Integer(data))
	     END IF
		  
	  CASE "vari_codigo"
		  IF NoExisteVariedad(dw_6.Object.espe_codigo[1],Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  END IF
		  
	  CASE "cpdd_calibr"
		  IF NoExisteCalibre(dw_6.Object.espe_codigo[1],dw_6.Object.vari_codigo[1],data) THEN
			  This.SetItem(1, ls_Columna, ls_nula)
			  RETURN 1
		  END IF
		  
		  
		CASE "emba_codigo"
		  IF NoExisteEmbalaje(dw_1.Object.clie_codigo[1],data) THEN
			  This.SetItem(1, ls_Columna, ls_nula)
			  RETURN 1
		  END IF
		  
		CASE "etiq_codigo"
		  IF NoExisteEtiqueta(Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  END IF
		  

END CHOOSE
end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3077
integer height = 980
long backcolor = 12632256
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
integer x = 718
integer y = 92
integer width = 1623
integer height = 832
integer taborder = 11
string dataobject = "dw_mant_planillasdestinodet_embalaje"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula
SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
	CASE  "cpdd_detpal","cpdd_rotula","cpdd_matint","cpdd_nropaq","cpdd_calemb"
 		  DatosEmbalaje(ls_columna,data)
		  IF ii_valor=2 THEN
			  This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			  Setnull(ii_valor)
			RETURN 1
   	  END IF	
	
END CHOOSE
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

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

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3077
integer height = 980
long backcolor = 12632256
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
integer x = 293
integer y = 56
integer width = 2533
integer height = 880
integer taborder = 11
string dataobject = "dw_mant_planillasdestinodet_calidad"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;//String	ls_Columna, ls_Nula, ls_resolucioncaja
//Integer	li_Nula
//
//SetNull(ls_Nula)
//SetNull(li_Nula)
//
//ls_Columna	=	dwo.Name
//
//CHOOSE CASE ls_Columna			
//	CASE "ccpd_cobay1","ccpd_cobay2","ccpd_cobay3","ccpd_cobay4"
// 		IF Not CienporCiento(ls_columna,data)	THEN 
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//			
//	CASE "ccpd_tamayo","ccpd_tamba1","ccpd_tamba2","ccpd_tamba3","ccpd_tamba4","ccpd_tameno"
// 		IF Not  CienxCiento(ls_columna,data)	THEN
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//
//	CASE "ccpd_promba"
// 		  Pesoproba(ls_columna,data)
//		  IF Dec(Data) > 99.9 OR Dec(Data) < 0  OR ii_valor=6 THEN			
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))	
//			setnull(ii_valor)
//			RETURN 1
//  		  END IF
//			
//	CASE "ccpd_forade"
//		
//		IF Not Sumaracimo(1, data) THEN 
//			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_forade",li_nula)			
//			RETURN 1 
//		END IF 
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1	
//		END IF
//		
//	CASE "ccpd_foraap"	
//		IF Not Sumaracimo(2, data) THEN 
//			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_foraap",li_nula)			
//			RETURN 1 
//		END IF 
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1	
//		END IF
//
//	CASE "ccpd_pesras"
//		IF Not Sumpesoracim(1, data) THEN 
//			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_pesras",li_nula)			
//			RETURN 1 
//		END IF 
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1	
//		END IF
//		
//	CASE "ccpd_pesrab"	
//		IF Not Sumpesoracim(2, data) THEN 
//			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_pesrab",li_nula)			
//			RETURN 1 
//		END IF 					
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1	
//		END IF
//		
//	CASE	"ccpd_residu"
//		
//		IF Integer(Data)	>	100	THEN
//			MessageBox("Atención","Residuos no puede ser mayor a 100")
//			dw_Calidad.SetITem(il_Fila,"ccpd_residu",li_Nula)
//			dw_Calidad.SetColumn("ccpd_residu")
//			RETURN 1
//		END IF
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1	
//		END IF
//		
//	CASE "ccpd_mancha"
//		
//		IF	Integer(Data)	>	100	THEN
//			MessageBox("Atención","Manchas en la Pien no puede ser Mayor a 100")
//			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
//			dw_Calidad.SetColumn("ccpd_mancha")
//			RETURN 1
//		END IF
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1	
//		END IF
//		
//	CASE "ccpd_insect"
//		
//		IF	Not ExisteDañoInsecto(Data)	THEN
//			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)			
//			dw_Calidad.SetColumn("ccpd_insect")
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_matext"
//		
//		IF	Not Existe_Matextraña(Data)	THEN
//			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)			
//			dw_Calidad.SetColumn("ccpd_matext")
//			RETURN 1
//		END IF		
//		
//	CASE "ccpd_rescal"
//		IF DatosResolucion(ls_columna,data) = False THEN
//			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
//			  RETURN 1
//		ELSE
//			IF data<>'M' THEN
//				dw_3.Object.ccpd_cobjca.protect = 1
//				dw_3.setcolumn("ccpd_grdobr")
//				dw_3.setfocus()
//			ELSE
//				dw_3.Object.ccpd_cobjca.protect = 0
//				dw_3.setcolumn("ccpd_cobjca")
//				dw_3.setfocus()
//			END IF
//			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
//			IF ls_resolucioncaja = 'A' THEN
//				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0
//				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')				  
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
//			
//			ELSEIF ls_resolucioncaja = 'O' THEN
//				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 1
//				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'O')
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 0             
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
//			ELSE 
//				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0  
//				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
//			END IF
//		END IF
//		
//	CASE "ccpd_grdobr"
//		IF Dec(Data) > 99.99 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_cobjca"
//		IF NOT existe_causal(Integer(data)) THEN
//			dw_2.Setitem(row, "ccpd_cobjca",integer(ls_Nula))				
//			RETURN 1 
//		END IF 
//END CHOOSE
//
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

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
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
integer width = 3077
integer height = 980
long backcolor = 12632256
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
integer x = 37
integer y = 84
integer width = 2985
integer height = 848
integer taborder = 11
string dataobject = "dw_mant_planillasdestinodet_condicion"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;//Integer	li_Nula, li_punuba, li_pununi, li_punura
//String	ls_Nula, ls_Columna, ls_resolucioncaja
//
//SetNull(li_Nula)
//SetNull(ls_Nula)
//
//ls_Columna	=	dwo.name
//
//CHOOSE CASE ls_Columna
//		
//	CASE "ccpd_deslev"
//		IF Not Sumadeshi(1, data) THEN 
//			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccpd_deslev",li_nula)			
//			RETURN 1 
//		END IF 
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_desmod"	
//		IF Not Sumadeshi(2, data) THEN 
//			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccpd_desmod",li_nula)			
//			RETURN 1 
//		END IF 
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_desalt"
//		IF Not Sumadeshi(3, data) THEN 
//			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccpd_desalt",li_nula)			
//			RETURN 1 
//		END IF
//		
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1
//		END IF
//	
//	CASE "ccpd_punuba"
//		ii_punuba	=	Integer(Data)		
//		
//
//	CASE "ccpd_pununi"
//		ii_pununi	=	Integer(Data)
//
//	CASE "ccpd_punura"	
//		ii_punura	=	Integer(Data)			
//		
//	CASE "ccpd_coblan"			
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_cocris"
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_codesp"
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_conso2"
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_cobapa"
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1
//		END IF
//		
//	CASE "ccpd_codesg"
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1
//		END IF
//		
//	CASE 	"ccpd_baacuo"
//		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
//			RETURN 1		
//		END IF
//		
//	CASE "ccpd_rescon"
//		IF DatosResolucion(ls_columna,data) = False THEN
//			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
//			  RETURN 1
//		ELSE
//			IF data<>'M' THEN
//				dw_4.Object.ccpd_cobjco.protect = 1
//				dw_4.setcolumn("ccpd_deslev")
//				dw_4.setfocus()
//			ELSE
//				dw_4.Object.ccpd_cobjco.protect = 0
//				dw_4.setcolumn("ccpd_cobjco")
//				dw_4.setfocus()
//			END IF
//			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
//			IF ls_resolucioncaja = 'A' THEN
//				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0
//				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')				  
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
//			
//			ELSEIF ls_resolucioncaja = 'O' THEN
//				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 1
//				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'O')
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 0             
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
//			ELSE 
//				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0  
//				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
//				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
//			END IF
//		END IF
//		
//	CASE "ccpd_cobjco"
//		IF NOT existe_causal(Integer(data)) THEN
//			dw_2.Setitem(row, "ccpd_cobjco",integer(ls_Nula))				
//			RETURN 1 
//		END IF 
//		
//END CHOOSE
//
//
//
//
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;//String	ls_Nula
//Decimal	ldc_Valor	
//
//IF IsValid(w_main) THEN	
//	w_main.SetMicroHelp(This.Tag)
//END IF
//
//SetNull(ls_Nula)
//
//CHOOSE CASE is_Columna
//	CASE "ccpd_punura"
//		IF dwo.Name <> "ccpd_punura" THEN 
//			IF Not IsNull(dw_4.Object.ccpd_punuba[il_fila]) OR &
//				Not IsNull(dw_4.Object.ccpd_pununi[il_fila])	THEN
//				IF  IsNull(dw_4.Object.ccpd_punura[il_fila]) THEN 
//						Messagebox("Atención","Debe ingresar Número de Racimo",StopSign!)							
//						SetColumn("ccpd_punura")						
//						RETURN 1
//						is_Columna	=	"" 
//				ELSE
//					is_Columna = dwo.name
//				END IF 
//			END IF
//		END IF 
//	CASE ELSE
//		is_Columna	=	dwo.Name
//
//
//END CHOOSE
//
//ib_datos_ok = true
//if rowcount() < 1 or getrow() = 0 or ib_borrar then 
//	ib_datos_ok = false
//else
//	SetRow(il_fila)
//	ScrolltoRow(il_fila)
//end if
end event

event losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;//ib_datos_ok = true
//if rowcount() < 1 or getrow() = 0 or ib_borrar then 
//	ib_datos_ok = false
//else
//	SetRow(il_fila)
//	ScrolltoRow(il_fila)
//end if
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
integer width = 3077
integer height = 980
long backcolor = 12632256
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
integer x = 805
integer y = 164
integer width = 1477
integer height = 720
integer taborder = 11
string dataobject = "dw_mant_planillasdestinodet_resolucion"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;//ib_datos_ok = true
//if rowcount() < 1 or getrow() = 0 or ib_borrar then 
//	ib_datos_ok = false
//else
//	SetRow(il_fila)
//	ScrolltoRow(il_fila)
//end if
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

