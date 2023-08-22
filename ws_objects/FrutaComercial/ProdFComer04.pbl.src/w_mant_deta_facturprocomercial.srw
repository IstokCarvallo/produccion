$PBExportHeader$w_mant_deta_facturprocomercial.srw
forward
global type w_mant_deta_facturprocomercial from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_facturprocomercial from w_mant_detalle_csd
integer width = 2679
integer height = 1624
end type
global w_mant_deta_facturprocomercial w_mant_deta_facturprocomercial

type variables
DataWindowChild	idwc_especies, idwc_productores, idwc_clientes, idwc_planta, idwc_variedades, idwc_categoria
uo_plantadesp	iuo_plantadesp
uo_variedades	iuo_variedades
uo_categorias	iuo_categoria

end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function decimal valpor (decimal ad_valor, integer ai_opcion, long al_fila)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long	ll_fila
String	ls_cliente, ls_planta, ls_productor, ls_especie, ls_variedad, ls_categoria, ls_semana

ls_cliente	 	 =	String(dw_1.Object.clie_codigo[il_fila])
ls_planta	 	 =	String(dw_1.Object.plde_codigo[il_fila])
ls_productor =	String(dw_1.Object.prod_codigo[il_fila])
ls_especie	 =	String(dw_1.Object.espe_codigo[il_fila])
ls_variedad	 =	String(dw_1.Object.vari_codigo[il_fila])
ls_categoria	 =	String(dw_1.Object.cate_codigo[il_fila])
ls_semana	 =	String(dw_1.Object.fpfc_semana[il_fila])


CHOOSE CASE as_Columna
		
 	CASE "clie_codigo"
		ls_cliente		=	as_Valor	
			
  	CASE "plde_codigo"
		ls_planta  		=	as_Valor	
		
	CASE "prod_codigo"
		ls_productor	=	as_Valor		
		
	CASE "espe_codigo"
		ls_especie	   =	as_Valor	
		
	CASE "vari_codigo"
		ls_variedad	   =	as_Valor	
		
	Case 'cate_codigo'
		ls_categoria = as_valor

	Case 'fpfc_semana'
		ls_semana = as_valor
		
END CHOOSE

	
	ll_fila	= dw_1.Find("clie_codigo = " + ls_cliente + " AND " + &
								"plde_codigo = " + ls_planta + " AND " + &
								"prod_codigo = " + ls_productor + " AND " + &
								"espe_codigo = " + ls_especie + " AND " + &
								"vari_codigo = " + ls_variedad + " And " + &	
								"fpfc_semana = " + ls_semana + " And " + &	
								"cate_codigo = " + ls_categoria, &	
								1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function decimal valpor (decimal ad_valor, integer ai_opcion, long al_fila);Dec{2} ld_resultado
/*
opcion 1 : Porcentaje Aplicado
opcion 2 : Precio Propuesto
*/
IF ai_opcion = 1 THEN
   ad_valor = Round(ad_valor/100,2)
	ld_resultado = (dw_1.Object.fpfc_prepro[al_Fila]  *  ad_valor) 
ELSE 
	ld_resultado = (dw_1.Object.fpfc_prepro[al_Fila] - ad_valor)
END IF

RETURN ld_resultado
end function

on w_mant_deta_facturprocomercial.create
call super::create
end on

on w_mant_deta_facturprocomercial.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[2]  = String(dw_1.GetItemNumber(il_fila, "clie_codigo"))
ias_campo[3]  = String(dw_1.GetItemNumber(il_fila, "plde_codigo"))
ias_campo[4]  = String(dw_1.GetItemNumber(il_fila, "vari_codigo"))	
ias_campo[5]  = String(dw_1.GetItemDate(il_fila, "fpfc_mespro"))
ias_campo[6]  = String(dw_1.GetItemNumber(il_fila, "fpfc_kilven"))	
ias_campo[7]  = String(dw_1.GetItemNumber(il_fila, "fpfc_prepro"))	
ias_campo[8]  = String(dw_1.GetItemNumber(il_fila, "fpfc_porcen"))	
ias_campo[9]  = String(dw_1.GetItemNumber(il_fila, "fpfc_preppt"))	
ias_campo[10] = String(dw_1.GetItemNumber(il_fila, "fpfc_valpor"))
ias_campo[11] = String(dw_1.GetItemNumber(il_fila, "cate_codigo"))
ias_campo[12]	= String(dw_1.GetItemNumber(il_fila, "fpfc_semana"))
	
If istr_Mant.Agrega Then
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
ElseIf Not istr_mant.Agrega And Not istr_mant.Borra Then
	dw_1.Object.clie_codigo.Protect				=	1
	dw_1.Object.plde_codigo.Protect				=	1
	dw_1.Object.vari_codigo.Protect				=	1
	dw_1.Object.fpfc_mespro.Protect				=	1
	dw_1.Object.fpfc_kilven.Protect				=	1
	dw_1.Object.fpfc_prepro.Protect				=	1
	dw_1.Object.fpfc_valpor.Protect				=	1
	dw_1.Object.fpfc_semana.Protect				=	1
	dw_1.Object.cate_codigo.Protect				=	1
	
	dw_1.Object.clie_codigo.Color	=	RGB(255,255,255)
	dw_1.Object.plde_codigo.Color	=	RGB(255,255,255)
	dw_1.Object.vari_codigo.Color	=	RGB(255,255,255)
	dw_1.Object.fpfc_mespro.Color=	RGB(255,255,255)
	dw_1.Object.fpfc_kilven.Color	=	RGB(255,255,255)
	dw_1.Object.fpfc_prepro.Color	=	RGB(255,255,255)
	dw_1.Object.fpfc_valpor.Color	=	RGB(255,255,255)
	dw_1.Object.cate_codigo.Color	=	RGB(255,255,255)
	dw_1.Object.fpfc_semana.Color=	RGB(255,255,255)
	
	dw_1.Object.clie_codigo.BackGround.Color	=	553648127
	dw_1.Object.plde_codigo.BackGround.Color	=	553648127
	dw_1.Object.vari_codigo.BackGround.Color	=	553648127
	dw_1.Object.fpfc_mespro.BackGround.Color=	553648127
	dw_1.Object.fpfc_kilven.BackGround.Color	=	553648127
	dw_1.Object.fpfc_prepro.BackGround.Color	=	553648127
	dw_1.Object.fpfc_valpor.BackGround.Color	=	553648127
	dw_1.Object.cate_codigo.BackGround.Color	=	553648127
	dw_1.Object.fpfc_semana.BackGround.Color=	553648127
End If

dw_1.SetFocus()
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "fpfc_mespro", Date(ias_campo[5]))
	dw_1.SetItem(il_fila, "fpfc_kilven", Dec(ias_campo[6]))
	dw_1.SetItem(il_fila, "fpfc_prepro", Dec(ias_campo[7]))
	dw_1.SetItem(il_fila, "fpfc_porcen", Dec(ias_campo[8]))
	dw_1.SetItem(il_fila, "fpfc_preppt", Dec(ias_campo[9]))
	dw_1.SetItem(il_fila, "fpfc_valpor", Dec(ias_campo[10]))
	dw_1.SetItem(il_fila, "cate_codigo", Integer(ias_campo[11]))
	dw_1.SetItem(il_fila, "fpfc_semana", integer(ias_campo[12]))
END IF
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "vari_codigo")) OR dw_1.GetItemNumber(il_fila, "vari_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nVariedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "cate_codigo")) OR dw_1.GetItemNumber(il_fila, "cate_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCategoria"
	ls_colu[li_cont]	= "cate_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fpfc_semana")) OR dw_1.GetItemNumber(il_fila, "fpfc_semana") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nSemana"
	ls_colu[li_cont]	= "fpfc_semana"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "plde_codigo")) OR dw_1.GetItemNumber(il_fila, "plde_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPlanta"
	ls_colu[li_cont]	= "plde_codigo"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fpfc_kilven")) OR dw_1.GetItemNumber(il_fila, "fpfc_kilven") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nKilos Vendidos"
	ls_colu[li_cont]	= "fpfc_kilven"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fpfc_prepro")) OR dw_1.GetItemNumber(il_fila, "fpfc_prepro") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Promedio"
	ls_colu[li_cont]	= "fpfc_prepro"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fpfc_porcen")) OR dw_1.GetItemNumber(il_fila, "fpfc_porcen") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPorcentaje Aplicado"
	ls_colu[li_cont]	= "fpfc_porcen"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fpfc_preppt")) OR dw_1.GetItemNumber(il_fila, "fpfc_preppt") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPrecio Propuesto"
	ls_colu[li_cont]	= "fpfc_preppt"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "fpfc_valpor")) OR dw_1.GetItemNumber(il_fila, "fpfc_valpor") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nValor Porcentaje"
	ls_colu[li_cont]	= "fpfc_valpor"
END IF

IF Isnull(dw_1.GetItemDate(il_fila, "fpfc_mespro")) THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nMes proceso"
	ls_colu[li_cont]	= "fpfc_mespro"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "prod_codigo", Long(istr_mant.argumento[1]))
end event

event open;x	= 100
y	= 450
PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm
dw_1.SetTransObject(sqlca)

iuo_variedades	= Create uo_Variedades
iuo_plantadesp	= Create uo_plantadesp
iuo_categoria	=	Create uo_categorias

dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(sqlca)
idwc_categoria.Retrieve()

dw_1.GetChild("espe_codigo",idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve(Integer(istr_mant.argumento[2]))

dw_1.GetChild("prod_codigo",idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(Integer(istr_mant.argumento[1]))

dw_1.GetChild("clie_codigo",idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
idwc_clientes.Retrieve(0)

dw_1.GetChild("vari_codigo",idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(Integer(istr_mant.argumento[2]),0)

dw_1.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(0)

istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_facturprocomercial
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_facturprocomercial
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_facturprocomercial
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_facturprocomercial
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_facturprocomercial
integer x = 2313
integer y = 376
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_facturprocomercial
integer x = 2313
integer y = 160
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_facturprocomercial
integer x = 2313
integer y = 592
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_facturprocomercial
integer x = 82
integer y = 92
integer width = 2139
integer height = 1396
string dataobject = "dw_mant_facturprocomercial"
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_fila
String	ls_columna, ls_null

ls_columna = GetColumnName()
SetNull(ls_null)

CHOOSE CASE ls_columna
		
	CASE "vari_codigo"
		IF Not iuo_variedades.Existe(Integer(Istr_mant.argumento[2]),Integer(Data),True,SqlCa)&
		 OR Duplicado(ls_columna,Data) THEN
			This.SetItem(il_fila,ls_columna,Integer(ls_null))
			RETURN 1
		END IF

	CASE "cate_codigo"
		IF Not iuo_categoria.Existe(Integer(Data),True,SqlCa)&
		 OR Duplicado(ls_columna,Data) THEN
			This.SetItem(il_fila,ls_columna,Integer(ls_null))
			RETURN 1
		END IF
		
	CASE "fpfc_semana"
		IF Duplicado(ls_columna,Data) THEN
			This.SetItem(il_fila,ls_columna,Integer(ls_null))
			RETURN 1
		END IF
		
	CASE "plde_codigo"
		IF Not iuo_plantadesp.Existe(Integer(Data),True,SqlCa)OR Duplicado(ls_columna,Data) THEN
			This.SetItem(il_fila,ls_columna,Integer(ls_null))
			RETURN 1
		END IF

  CASE "fpfc_porcen"
	   dw_1.Object.fpfc_valpor[il_fila] = valpor(Dec(Data),1,il_fila)
	   dw_1.Object.fpfc_preppt[il_fila] = dw_1.Object.preppt[il_fila]		
				
  CASE "fpfc_preppt"	
	   dw_1.Object.fpfc_valpor[il_fila] = valpor(Dec(Data),2,il_fila)
//		el valor del porcentaje se asigna en el losefocus()
     
END CHOOSE
end event

event dw_1::losefocus;call super::losefocus;dw_1.Object.fpfc_porcen[getrow()] = dw_1.Object.porcen[getrow()]
end event

