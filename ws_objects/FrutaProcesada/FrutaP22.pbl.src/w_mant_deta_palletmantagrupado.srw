$PBExportHeader$w_mant_deta_palletmantagrupado.srw
forward
global type w_mant_deta_palletmantagrupado from w_mant_detalle
end type
type dw_2 from datawindow within w_mant_deta_palletmantagrupado
end type
type dw_3 from datawindow within w_mant_deta_palletmantagrupado
end type
end forward

global type w_mant_deta_palletmantagrupado from w_mant_detalle
integer width = 4197
integer height = 1796
string title = "CAMBIO DE DATOS"
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_deta_palletmantagrupado w_mant_deta_palletmantagrupado

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	&
                  dw_plantadesp	,	dw_especies		,	&
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente,&
						idwc_predio		, 	idwc_cuartel
						
Integer il_lote	

uo_calibre								iuo_calibre


end variables

forward prototypes
public function boolean noexisteproductor (string ls_columna)
public function boolean noexistepredio (long ai_productor, long ai_predio)
public function boolean noexistecuartel (long ai_productor, long ai_predio, long ai_cuartel)
public function boolean noexistevariedad (string as_columna, string as_valor)
public function boolean existepacking (integer ai_planta)
public function boolean existeembalaje (string as_embalaje, integer al_cliente)
public function boolean noexistecategoria (integer categoria)
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

public function boolean noexistevariedad (string as_columna, string as_valor);Integer	li_Especie, li_Variedad, li_Existes
Boolean	lb_Retorno

li_Especie	=	dw_1.Object.espe_codigo[1]

CHOOSE CASE as_Columna
	CASE "varireal"
		li_Variedad	=	Integer(as_Valor)
		
	CASE "variedad"
		li_Variedad	=	Integer(as_Valor)	
	
END CHOOSE

SELECT Count(*)
	INTO	 :li_Existes
	FROM	 dbo.variedades
	WHERE  vari_codigo	=	:li_Variedad
	AND    espe_codigo	=	:li_Especie;
	
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Variedades")
	ELSEIF 	li_Existes	=	0	THEN
		MessageBox("Atención","Código de Variedad no Existe.~r " +&
		"Ingrese Otro.")			
		lb_Retorno	=	TRUE
	END IF
	

RETURN lb_Retorno
end function

public function boolean existepacking (integer ai_planta);Integer	li_Existes
Boolean	lb_Retorno=FALSE

SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.plantadesp
WHERE  plde_codigo	=	:ai_planta;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de plantadesp")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Packing No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public function boolean existeembalaje (string as_embalaje, integer al_cliente);Integer	li_Existes
Boolean	lb_Retorno=FALSE


SELECT Count(*)
INTO	 :li_Existes
FROM	 dbo.embalajesprod
WHERE  emba_codigo	=	:as_embalaje
AND	 clie_codigo 	= 	:al_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de embalajesprod")
	lb_Retorno	=	TRUE
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Embalaje No Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

public function boolean noexistecategoria (integer categoria);Integer	li_cont
Boolean	lb_retorna = True

	SELECT  count(*) INTO :li_cont
	FROM    dbo.categorias
	WHERE	  cate_codigo = :categoria ;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Categorias")
		lb_Retorna = True
		
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Categoria no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		lb_Retorna = True
	ELSE
		lb_retorna = False
	END IF

Return lb_retorna
end function

on w_mant_deta_palletmantagrupado.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_mant_deta_palletmantagrupado.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_deshace;call super::ue_deshace;//dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
//dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
//dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
//dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event open;x	= 0
y	= 0

PostEvent("ue_recuperadatos")

iuo_calibre   						=	Create uo_calibre


istr_mant = Message.PowerObjectParm
istr_mant.argumento[5] = istr_mant.argumento[5]

IF istr_mant.argumento[11] = 'P' THEN
	dw_2.DataObject = "dw_mues_mantpaltrans"
	dw_3.DataObject = "dw_mues_mantpaltrans"		
	dw_1.DataObject = "dw_mues_palletfruta_mant_origen_caja"	
ELSE
	dw_2.DataObject = "dw_mues_variedadrotulada_despachados"		
	dw_3.DataObject = "dw_mues_variedadrotulada_despachados"	
	dw_1.DataObject = "dw_mues_palletfruta_rotula_origen_caja_desp"
END IF

dw_2.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_2)
istr_mant.dw2.ShareData(dw_3)


end event

event ue_recuperadatos;Integer	li_cliente,li_planta
Long		ll_folio, ll_fila, ll_fila1, ll_guia
String	ls_variedad, ls_embalaje, ls_productor, ls_calibre, ls_huerto, ls_cuartel, ls_packing, ls_fecha, &
			ls_cliente, ls_planta, ls_folio, ls_guia, ls_pallet
Date		ld_fecemb

li_cliente	=	Integer(istr_Mant.Argumento[1])
li_planta	=	Integer(istr_Mant.Argumento[2])
ll_guia		=	Long(istr_mant.argumento[10])

IF (IsNull(ll_guia) OR ll_guia = 0) THEN
	ll_folio 	=	Long(istr_mant.argumento[6])
	ls_folio 	=	istr_mant.argumento[6]
ELSE
	ll_folio 	=	ll_guia
	ls_folio 	=	istr_mant.argumento[10]

END IF

ls_cliente	=	istr_Mant.Argumento[1]
ls_planta	=	istr_Mant.Argumento[2]
ls_guia	 	=	istr_mant.argumento[10]

dw_1.Retrieve(li_cliente,li_planta,ll_folio)

//IF dw_3.RowCount() > 0 THEN
//		
//	FOR ll_fila = 1 TO dw_3.RowCount()
//			ls_pallet		=	string(dw_3.Object.paen_numero[ll_fila])
//			ls_variedad 	= 	string(dw_3.Object.pafr_varrot[ll_fila])
//			ls_embalaje 	= 	string(dw_3.Object.emba_codigo[ll_fila])
//			ls_productor 	= 	string(dw_3.Object.pafr_prdrot[ll_fila])		
//			ls_calibre 		= 	string(dw_3.Object.pafr_calrot[ll_fila])
//			ls_huerto		=	string(dw_3.Object.pafr_huert4[ll_fila])
//			ls_cuartel		=	string(dw_3.Object.pafr_cuart4[ll_fila])
//			ls_packing		=	string(dw_3.Object.pafr_rotpak[ll_fila])
//			ls_fecha			=	string(dw_3.Object.pafr_fecrot[ll_fila],'yyyy/mm/dd')
//			ld_fecemb 		=  dw_3.Object.pafr_fecemb[ll_fila]
//
//			
//			ll_fila1	= dw_1.Find( "clie_codigo = " + ls_cliente + &
//									 " AND plde_codigo = " + ls_planta + &
//									 " AND paen_numero = " + ls_pallet + &
//									 " AND pafr_varrot = " + ls_variedad + &
//									 " AND emba_codigo = '" + ls_embalaje + "'" + &
//									 " AND pafr_prdrot = " + ls_productor + &
//									 " AND pafr_calrot = '" + ls_calibre + "'" + &
//									 " AND pafr_huert4 = " + ls_huerto + &
//									 " AND pafr_cuart4 = " + ls_cuartel + &			
//									 " AND pafr_rotpak = " + ls_packing + &						
//									 " AND String(pafr_fecrot,'yyyy-mm-dd') = " + String(ls_fecha,'yyyy-mm-dd'), 1, istr_mant.dw.RowCount())
//	
////		IF ll_fila1 > 0 AND dw_1.Object.paen_numero[1] = dw_3.Object.paen_numero[ll_fila]  THEN
//		IF ll_fila1 > 0 THEN
//			dw_1.Object.variedad[ll_fila1] 	= dw_3.Object.pafr_varrot[ll_fila]
//			dw_1.Object.embalaje[ll_fila1] 	= dw_3.Object.emba_codigo[ll_fila]
//			dw_1.Object.productor[ll_fila1] 	= dw_3.Object.pafr_prdrot[ll_fila]
//			dw_1.Object.calibre[ll_fila1] 	= dw_3.Object.pafr_calrot[ll_fila]
//			dw_1.Object.huerto[ll_fila1] 		= dw_3.Object.pafr_huert4[ll_fila]
//			dw_1.Object.cuartel[ll_fila1] 	= dw_3.Object.pafr_cuart4[ll_fila]
//			dw_1.Object.packing[ll_fila1] 	= dw_3.Object.pafr_rotpak[ll_fila]
//			dw_1.Object.fecha[ll_fila1] 		= dw_3.Object.pafr_fecrot[ll_fila]			
//		END IF	
//									 
//	NEXT								 
//END IF	


end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_palletmantagrupado
boolean visible = false
long backcolor = 134217750
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_palletmantagrupado
boolean visible = false
long backcolor = 134217750
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_palletmantagrupado
boolean visible = false
long backcolor = 134217750
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_palletmantagrupado
boolean visible = false
long backcolor = 134217750
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_palletmantagrupado
boolean visible = false
integer x = 3936
integer y = 328
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_palletmantagrupado
integer x = 3931
integer y = 116
end type

event pb_acepta::clicked;Long		ll_fila, ll_fila1, ll_fila2, ll_fil
String	ls_variedad, ls_embalaje, ls_productor, ls_calibre, ls_huerto, ls_cuartel, ls_packing, ls_fecha, ls_embalajereal,&
			ls_cliente, ls_folio,  ls_planta, ls_guia,ls_varireal,ls_prodreal,ls_calireal,ls_huerreal,&
			ls_cuarreal,ls_packreal, ls_categoria, ls_catrotulado	
Date		ld_fecemb, ld_fecha
Integer	li_prueba, li_categoria, li_catrotulado

ls_cliente	=	istr_Mant.Argumento[1]
ls_planta	=	istr_Mant.Argumento[2]
ls_folio 	=	istr_mant.argumento[6]
ls_guia		=	istr_mant.argumento[10]

FOR ll_fila = 1 TO dw_1.RowCount()
	IF isnull(dw_1.Object.varireal[ll_fila]) OR dw_1.Object.varireal[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nueva Variedad Real.", &
					Exclamation!, OK!)
		Return 1					
	END IF
	
	IF isnull(dw_1.Object.variedad[ll_fila]) OR dw_1.Object.variedad[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nueva Variedad Rotulada.", &
					Exclamation!, OK!)
		Return 1					
	END IF	

	IF isnull(dw_1.Object.embalaje[ll_fila]) OR dw_1.Object.embalaje[ll_fila] = '' THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Embalaje Rotulado.", &
					Exclamation!, OK!)
		Return 1			
	END IF
	
	IF isnull(dw_1.Object.embalajereal[ll_fila]) OR dw_1.Object.embalajereal[ll_fila] = '' THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Embalaje Real.", &
					Exclamation!, OK!)
		Return 1			
	END IF
	
	IF isnull(dw_1.Object.prodreal[ll_fila]) OR dw_1.Object.prodreal[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Productor Real.", &
					Exclamation!, OK!)
		Return 1					
	END IF
	
	IF isnull(dw_1.Object.productor[ll_fila]) OR dw_1.Object.productor[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Productor Rotulado.", &
					Exclamation!, OK!)
		Return 1					
	END IF
	
	IF isnull(dw_1.Object.calreal[ll_fila]) OR dw_1.Object.calreal[ll_fila] = '' THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Calibre Real.", &
					Exclamation!, OK!)
		Return 1			
	END IF
	
	IF isnull(dw_1.Object.calibre[ll_fila]) OR dw_1.Object.calibre[ll_fila] = '' THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Calibre Rotulado.", &
					Exclamation!, OK!)
		Return 1			
	END IF	
	
	IF isnull(dw_1.Object.packreal[ll_fila]) OR dw_1.Object.packreal[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Packing Real.", &
					Exclamation!, OK!)
		Return 1					
	END IF

	IF isnull(dw_1.Object.packing[ll_fila]) OR dw_1.Object.packing[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Packing Rotulado.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
		
	IF isnull(dw_1.Object.huerreal[ll_fila]) OR dw_1.Object.huerreal[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Predio Real.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
	
	IF isnull(dw_1.Object.huerto[ll_fila]) OR dw_1.Object.huerto[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Predio Rotulado.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
	
	IF isnull(dw_1.Object.cuartreal[ll_fila]) OR dw_1.Object.cuartreal[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Cuartel Real.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
	
	IF isnull(dw_1.Object.cuartel[ll_fila]) OR dw_1.Object.cuartel[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de Nuevo Cuartel Rotulado.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
	
	
	IF isnull(dw_1.Object.catereal[ll_fila]) OR dw_1.Object.catereal[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de la Nueva Categoria.", &
					Exclamation!, OK!)
		Return 1					
	END IF	
	
	IF isnull(dw_1.Object.caterotulada[ll_fila]) OR dw_1.Object.caterotulada[ll_fila] = 0 THEN
		MessageBox("Atención", "Falta el Ingreso de la Nueva Categoria Rotulado.", &
					Exclamation!, OK!)
		Return 1					
	END IF	

NEXT	

FOR ll_fila1 = 1 TO dw_1.RowCount()
	FOR ll_fila2 = 1 TO dw_2.RowCount() 
		
		ls_variedad 	= 	string(dw_1.Object.pafr_varrot[ll_fila1])
		ls_embalaje		= 	dw_1.Object.emba_codigo[ll_fila1]
	   ls_productor	= 	string(dw_1.Object.pafr_prdrot[ll_fila1])		
		ls_calibre 		= 	dw_1.Object.pafr_calrot[ll_fila1]		
	   ls_huerto		= 	string(dw_1.Object.pafr_huert4[ll_fila1])				
	   ls_cuartel		= 	string(dw_1.Object.pafr_cuart4[ll_fila1])						
	   ls_packing		= 	string(dw_1.Object.pafr_rotpak[ll_fila1])	
					
		ls_varireal 	= 	string(dw_1.Object.vari_codigo[ll_fila1])
		ls_prodreal		= 	string(dw_1.Object.prod_codigo[ll_fila1])		
		ls_calireal		= 	dw_1.Object.pafr_calibr[ll_fila1]		
	   ls_huerreal		= 	string(dw_1.Object.pafr_huert1[ll_fila1])				
	   ls_cuarreal		= 	string(dw_1.Object.pafr_cuart1[ll_fila1])						
	   ls_packreal		= 	string(dw_1.Object.pafr_copack[ll_fila1])	
		ls_embalajereal=  dw_1.Object.pafr_embrea[ll_fila1]
		
		ls_categoria	= 	String(dw_1.Object.cate_codigo[ll_fila1])		
		ls_catrotulado	= 	String(dw_1.Object.pafr_catrot[ll_fila1])	
		
		ls_folio			=	string(dw_1.Object.paen_numero[ll_fila1])
			
//		IF Isnull(dw_1.Object.pafr_huert4[ll_fila1]) THEN
//			ls_huerto='1'
//		END IF
//		IF Isnull(dw_1.Object.pafr_cuart4[ll_fila1]) THEN
//			ls_cuartel='1'
//		END IF
				
		ll_fila	= dw_2.Find( "clie_codigo = " + ls_cliente + &
									 " AND plde_codigo = " + ls_planta + &
									 " AND paen_numero = " + ls_folio + &
									 " AND pafr_varrot = " + ls_variedad + &
									 " AND emba_codigo = '" + ls_embalaje + "'" + &
									 " AND pafr_embrea = '" + ls_embalajereal + "'" + &
									 " AND pafr_huert4 = " + ls_huerto + &
									 " AND pafr_cuart4 = " + ls_cuartel + &											 
									 " AND pafr_prdrot = " + ls_productor + &
									 " AND pafr_calrot = '" + ls_calibre + "'" + &
									 " AND pafr_rotpak = " + ls_packing + &	
									 " AND vari_codigo = " + ls_varireal + &
									 " AND pafr_huert1 = " + ls_huerreal + &
									 " AND pafr_cuart1 = " + ls_cuarreal + &	
									 " AND cate_codigo = " + ls_categoria + &
									 " AND prod_codigo = " + ls_prodreal + &
									 " AND pafr_calibr = '" + ls_calireal + "'" + &
									 " AND pafr_copack = " + ls_packreal , 1, dw_2.RowCount())
									       
		IF ll_fila > 0 THEN

			dw_2.Object.vari_codigo[ll_fila] = dw_1.Object.varireal[ll_fila1]
			dw_2.Object.pafr_varrot[ll_fila] = dw_1.Object.variedad[ll_fila1]
			
			dw_2.Object.cate_codigo[ll_fila] = dw_1.Object.catereal[ll_fila1]
			dw_2.Object.pafr_catrot[ll_fila] = dw_1.Object.caterotulada[ll_fila1]
			
			dw_2.Object.emba_codigo[ll_fila] = dw_1.Object.embalaje[ll_fila1]
			dw_2.Object.pafr_embrea[ll_fila] = dw_1.Object.embalajereal[ll_fila1]
			
			dw_2.Object.prod_codigo[ll_fila] = dw_1.Object.prodreal[ll_fila1]
			dw_2.Object.pafr_prdrot[ll_fila] = dw_1.Object.productor[ll_fila1]
			
			ls_calibre = dw_1.Object.calreal[ll_fila1]
			dw_2.Object.pafr_calibr[ll_fila] = iuo_calibre.fomato_calibre(ls_calibre,sqlca)
			ls_calibre = dw_1.Object.calibre[ll_fila1]
			dw_2.Object.pafr_calrot[ll_fila] = iuo_calibre.fomato_calibre(ls_calibre,sqlca)
			
			dw_2.Object.pafr_huert1[ll_fila] = dw_1.Object.huerreal[ll_fila1]
			dw_2.Object.pafr_cuart1[ll_fila] = dw_1.Object.cuartreal[ll_fila1]
			
			IF isnull(dw_1.Object.huerto[ll_fila1]) OR dw_1.Object.huerto[ll_fila1] = 0 THEN
				dw_2.Object.pafr_huert4[ll_fila] = 1
			ELSE	
				dw_2.Object.pafr_huert4[ll_fila] = dw_1.Object.huerto[ll_fila1]
			END IF
			
			IF isnull(dw_1.Object.cuartel[ll_fila1]) OR dw_1.Object.cuartel[ll_fila1] = 0 THEN
				dw_2.Object.pafr_cuart4[ll_fila] = 1
			ELSE
				dw_2.Object.pafr_cuart4[ll_fila] = dw_1.Object.cuartel[ll_fila1]
			END IF	
			
			dw_2.Object.pafr_copack[ll_fila] = dw_1.Object.packreal[ll_fila1]
			dw_2.Object.pafr_rotpak[ll_fila] = dw_1.Object.packing[ll_fila1]			
			
		END IF	
								 
	NEXT							 
NEXT		

MessageBox("Atención", "Cajas Aplicadas a Detalle.", &
					Exclamation!, OK!)

end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_palletmantagrupado
integer x = 3936
integer y = 548
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_palletmantagrupado
integer x = 32
integer y = 24
integer width = 3803
integer height = 1592
string dataobject = "dw_mues_palletfruta_mant_origen_caja"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Integer	li_especie, li_variedad


SetNull(ls_Nula)

ls_columna = dwo.name


CHOOSE CASE ls_columna
		
	CASE "variedad"
		IF NoExisteVariedad(ls_Columna,Data)	THEN
			dw_1.SetItem(row,ls_Columna,Integer(ls_columna))
			RETURN 1
		END IF	
		
	CASE "varireal"
		IF NoExisteVariedad(ls_Columna,Data)	THEN
			dw_1.SetItem(row,ls_Columna,Integer(ls_columna))
			RETURN 1
		END IF		
			
	CASE "calibre"
		
		li_especie 	= dw_1.Object.espe_codigo[row]
		li_variedad = dw_1.Object.pafr_varrot[row]
		
		IF NOT iuo_calibre.existe(li_especie,li_variedad,data,True,sqlca) THEN
			dw_1.SetItem(Row, ls_columna, Upper(ls_Nula))
			RETURN 1
		ELSE	
			dw_1.Object.calibre[row] = iuo_calibre.calibre
		END IF	
		
	CASE "calreal"
		
		li_especie 	= dw_1.Object.espe_codigo[row]
		li_variedad = dw_1.Object.vari_codigo[row]
		
		IF NOT iuo_calibre.existe(li_especie,li_variedad,data,True,sqlca) THEN
			dw_1.SetItem(Row, ls_columna, Upper(ls_Nula))
			RETURN 1
		ELSE	
			dw_1.Object.calreal[row] = iuo_calibre.calibre
		END IF	
				
	CASE "prodreal"	
		IF  NoExisteProductor(data) THEN
			dw_1.SetItem(row, ls_columna, Long(ls_Nula))
			RETURN 1
		ELSE
			dw_1.SetItem(row, "huerreal", long(ls_nula))
			dw_1.SetItem(row, "cuartreal", long(ls_nula))
		END IF
		
	CASE "productor"	
		IF  NoExisteProductor(data) THEN
			dw_1.SetItem(row, ls_columna, Long(ls_Nula))
			RETURN 1
		ELSE
			dw_1.SetItem(row, "huerto", long(ls_nula))
			dw_1.SetItem(row, ls_columna, long(ls_nula))
		END IF
		
	CASE "huerreal"
		IF Not noexistepredio(dw_1.Object.prodreal[row],integer(data)) THEN
			dw_1.SetItem(row, ls_columna, integer(ls_nula))
			Return 1
		END IF		
		
	CASE "huerto"
		
		IF Not noexistepredio(dw_1.Object.productor[row],integer(data)) THEN
			dw_1.SetItem(row, ls_columna, integer(ls_nula))
			Return 1
		END IF	
		
	CASE "cuartreal"
		IF Not noexistecuartel(dw_1.Object.prodreal[row],dw_1.Object.huerreal[row],integer(data)) THEN
			dw_1.SetItem(row,ls_columna, long(ls_nula))
			Return 1
		END IF			
		
	CASE "cuartel"
		IF Not noexistecuartel(dw_1.Object.productor[row],dw_1.Object.huerto[row],integer(data)) THEN
			dw_1.SetItem(row,ls_columna, long(ls_nula))
			Return 1
		END IF	
		
	CASE 'embalaje'
		IF existeembalaje(Data,dw_1.Object.clie_codigo[row])	THEN
			dw_1.SetItem(il_Fila,ls_Columna,String(ls_nula))
			RETURN 1
		END IF
		
	CASE 'embalajereal'	
		IF existeembalaje(Data,dw_1.Object.clie_codigo[row])	THEN
			dw_1.SetItem(il_Fila,ls_Columna,String(ls_nula))
			RETURN 1
		END IF	
		
	CASE 'packing'
		IF existepacking(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE 'packreal'	
		IF existepacking(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(ls_nula))
			RETURN 1
		END IF	
			
	CASE 'catereal'
		IF noexistecategoria(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE 'caterotulada'	
		IF noexistecategoria(Integer(Data))	THEN
			dw_1.SetItem(il_Fila,ls_Columna,Integer(ls_nula))
			RETURN 1
		END IF		
		
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

end event

type dw_2 from datawindow within w_mant_deta_palletmantagrupado
boolean visible = false
integer x = 466
integer y = 1800
integer width = 3538
integer height = 920
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_mantpaltrans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_mant_deta_palletmantagrupado
boolean visible = false
integer x = 1646
integer y = 1952
integer width = 2176
integer height = 668
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_mantpaltrans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

