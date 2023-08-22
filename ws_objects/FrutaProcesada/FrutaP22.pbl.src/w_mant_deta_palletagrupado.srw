$PBExportHeader$w_mant_deta_palletagrupado.srw
forward
global type w_mant_deta_palletagrupado from w_mant_detalle
end type
type dw_2 from datawindow within w_mant_deta_palletagrupado
end type
type dw_3 from datawindow within w_mant_deta_palletagrupado
end type
end forward

global type w_mant_deta_palletagrupado from w_mant_detalle
integer width = 4334
integer height = 1900
string title = "CAMBIO DE DATOS"
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_deta_palletagrupado w_mant_deta_palletagrupado

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	&
                  dw_plantadesp	,	dw_especies		,	&
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente,&
						idwc_predio		, 	idwc_cuartel
						
Integer il_lote			

uo_calibre	iuo_calibre


end variables

forward prototypes
public function boolean noexisteproductor (string ls_columna)
public function boolean noexistepredio (long ai_productor, long ai_predio)
public function boolean noexistecuartel (long ai_productor, long ai_predio, long ai_cuartel)
end prototypes

public function boolean noexisteproductor (string ls_columna);String	ls_nombre
Integer	li_cliente
Boolean	lb_retorna = True
Long		ll_codigo

li_cliente	 = Integer(istr_mant.argumento[1])
ll_codigo 	 = Long(ls_columna)

	SELECT	prod_nombre INTO :ls_nombre
	FROM    dbo.productores
	WHERE	   prod_codigo = :ll_codigo ;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		lb_Retorna = True
		
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		lb_Retorna = True
	ELSE
		lb_retorna = False
	END IF

Return lb_retorna
end function

public function boolean noexistepredio (long ai_productor, long ai_predio);Integer	li_existe

SELECT Count(prod_codigo) 
INTO :li_existe
FROM dbo.spro_prodpredio
WHERE prod_codigo = :ai_productor
AND	prpr_codigo = :ai_predio;

IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prediosproductor")
	RETURN FALSE
ELSEIF li_existe>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Codigo de Predio no esta asignado a Productor.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF
end function

public function boolean noexistecuartel (long ai_productor, long ai_predio, long ai_cuartel);Integer	li_existe

SELECT Count(prod_codigo) 
INTO :li_existe
FROM dbo.spro_prodcuarteles
WHERE prod_codigo = :ai_productor
AND	prpr_codigo = :ai_predio
AND	prcc_codigo = :ai_cuartel;

IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prodcuarteles")
	RETURN FALSE
ELSEIF li_existe>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Codigo de Cuartel no esta asignado a Productor.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF
end function

on w_mant_deta_palletagrupado.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_mant_deta_palletagrupado.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_deshace;call super::ue_deshace;//dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
//dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
//dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
//dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm
istr_mant.argumento[5] = istr_mant.argumento[5]

dw_2.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_2)
istr_mant.dw2.ShareData(dw_3)

iuo_calibre   						=	Create uo_calibre


end event

event ue_recuperadatos;Integer	li_cliente,li_planta
Long		ll_folio, ll_fila, ll_fila1
String	ls_variedad, ls_embalaje, ls_productor, ls_calibre, ls_cliente, ls_planta, ls_folio
Date		ld_fecemb

ll_folio 	=	Long(istr_mant.argumento[2])
li_cliente	=	Integer(istr_Mant.Argumento[1])
li_planta	=	Integer(istr_Mant.Argumento[6])

ls_cliente	=	istr_Mant.Argumento[1]
ls_planta	=	istr_Mant.Argumento[6]
ls_folio 	=	istr_mant.argumento[2]

dw_1.Retrieve(li_cliente,li_planta,ll_folio)

IF dw_3.RowCount() > 0 THEN
		
	FOR ll_fila = 1 TO dw_3.RowCount()
			ls_variedad = string(dw_3.Object.pafr_varrot[ll_fila])
			ls_embalaje = string(dw_3.Object.emba_codigo[ll_fila])
			ls_productor = string(dw_3.Object.pafr_prdrot[ll_fila])		
			ls_calibre 	= string(dw_3.Object.pafr_calrot[ll_fila])	
			ld_fecemb =  dw_3.Object.pafr_fecemb[ll_fila]
			
			ll_fila1	= istr_mant.dw.Find( "clie_codigo = " + ls_cliente + &
									 " AND paen_numero = " + ls_folio + &
									 " AND vari_codigo = " + ls_variedad + &
									 " AND prod_codigo = " + ls_productor + &
									 " AND plde_codigo = " + ls_planta + &
									 " AND pafr_calibr = '" + ls_calibre + "'", 1, istr_mant.dw.RowCount())
	
		IF ll_fila1 > 0 AND dw_1.Object.paen_numero[1] = dw_3.Object.paen_numero[ll_fila]  THEN
			dw_1.Object.productor[ll_fila1] 	= dw_3.Object.prod_codigo[ll_fila]
			dw_1.Object.calibre[ll_fila1] 	= dw_3.Object.pafr_calibr[ll_fila]
			dw_1.Object.huerto[ll_fila1] 		= dw_3.Object.pafr_huert1[ll_fila]
			dw_1.Object.cuartel[ll_fila1] 	= dw_3.Object.pafr_cuart1[ll_fila]
		END IF	
									 
	NEXT								 
END IF	


end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_palletagrupado
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_palletagrupado
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_palletagrupado
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_palletagrupado
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_palletagrupado
boolean visible = false
integer x = 3927
integer y = 400
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_palletagrupado
integer x = 3927
integer y = 188
end type

event pb_acepta::clicked;Long	ll_fila, ll_fila1, ll_fila2, ll_fil
String	ls_embalaje, ls_productor, ls_cliente, ls_folio, ls_variedad, ls_planta, ls_calibre
Date		ld_fecemb
//Integer

ls_folio 	=	istr_mant.argumento[2]
ls_cliente	=	istr_Mant.Argumento[1]
ls_planta	=	istr_Mant.Argumento[6]

FOR ll_fila = 1 TO dw_1.RowCount()
	IF isnull(dw_1.Object.productor[ll_fila]) OR dw_1.Object.productor[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso del Productor.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
	
	IF isnull(dw_1.Object.calibre[ll_fila]) OR dw_1.Object.calibre[ll_fila] = '' THEN
		MessageBox("Atención", "Falta el Ingreso del Calibre.", &
					Exclamation!, OK!)
		Return 1			
	END IF	
NEXT	

FOR ll_fila1 = 1 TO dw_1.RowCount()
	FOR ll_fila2 = 1 TO dw_2.RowCount() 
		ls_variedad = string(dw_1.Object.vari_codigo[ll_fila1])
	   ls_productor = string(dw_1.Object.prod_codigo[ll_fila1])		
		ls_calibre 	= string(dw_1.Object.pafr_calibr[ll_fila1])		
			
		ll_fila	= dw_2.Find( "clie_codigo = " + ls_cliente + &
								 " AND paen_numero = " + ls_folio + &
								 " AND prod_codigo = " + ls_productor + &
								 " AND plde_codigo = " + ls_planta + &
								 " AND vari_codigo = " + ls_variedad + &
								 " AND pafr_calibr = '" + ls_calibre + "'", ll_fila2, dw_2.RowCount())
		IF ll_fila > 0 THEN
			dw_2.Object.prod_codigo[ll_fila] = dw_1.Object.productor[ll_fila1]
			ls_calibre = dw_1.Object.calibre[ll_fila1]
			
			dw_2.Object.pafr_calibr[ll_fila] = iuo_calibre.fomato_calibre(ls_calibre,sqlca)   
			
			IF isnull(dw_1.Object.huerto[ll_fila1]) OR dw_1.Object.huerto[ll_fila1] = 0 THEN
				dw_2.Object.pafr_huert1[ll_fila] = 1
			ELSE	
				dw_2.Object.pafr_huert1[ll_fila] = dw_1.Object.huerto[ll_fila1]
			END IF
			
			IF isnull(dw_1.Object.cuartel[ll_fila1]) OR dw_1.Object.cuartel[ll_fila1] = 0 THEN
				dw_2.Object.pafr_cuart1[ll_fila] = 1
			ELSE
				dw_2.Object.pafr_cuart1[ll_fila] = dw_1.Object.cuartel[ll_fila1]
			END IF	
		END IF	
								 
	NEXT							 
NEXT		


MessageBox("Atención", "Cajas Aplicadas a Detalle.", &
					Exclamation!, OK!)

end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_palletagrupado
integer x = 3927
integer y = 620
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_palletagrupado
integer x = 82
integer y = 84
integer width = 3803
integer height = 1592
string dataobject = "dw_mues_palletfruta_transitorio_origen_caja"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Integer	li_especie, li_variedad

SetNull(ls_Nula)

ls_columna = dwo.name


CHOOSE CASE ls_columna
		
	CASE "calibre"
		
		li_especie	= dw_1.Object.espe_codigo[row]
		li_variedad	= dw_1.Object.vari_codigo[row]
		
		IF NOT iuo_calibre.existe(li_especie,li_variedad,data,True,sqlca) THEN
			dw_1.SetItem(Row, ls_columna, Upper(ls_Nula))
			RETURN 1
		ELSE	
			dw_1.Object.calibre[row] 			= iuo_calibre.fomato_calibre(data,sqlca)
			dw_1.Object.pafr_calibr[il_fila] = iuo_calibre.fomato_calibre(data,sqlca)
			Return 1
		END IF	
			
	CASE "productor"	
		IF  NoExisteProductor(data) THEN
			dw_1.SetItem(row, ls_columna, Long(ls_Nula))
			RETURN 1
		ELSE
			dw_1.SetItem(row, "huerto", long(ls_nula))
			dw_1.SetItem(row, "cuartel", long(ls_nula))
		END IF
		
	CASE "huerto"
		
		IF Not noexistepredio(dw_1.Object.productor[row],integer(data)) THEN
			dw_1.SetItem(row, "huerto", integer(ls_nula))
			Return 1
		END IF	
		
	CASE "cuartel"
		
		IF Not noexistecuartel(dw_1.Object.productor[row],dw_1.Object.huerto[row],integer(data)) THEN
			dw_1.SetItem(row, "cuartel", long(ls_nula))
			Return 1
		END IF		
		
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

end event

type dw_2 from datawindow within w_mant_deta_palletagrupado
boolean visible = false
integer x = 229
integer y = 1696
integer width = 2391
integer height = 572
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_mant_deta_palletagrupado
boolean visible = false
integer x = 1541
integer y = 1660
integer width = 2176
integer height = 668
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

