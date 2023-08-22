$PBExportHeader$w_maed_spro_procvaciadocomercial.srw
forward
global type w_maed_spro_procvaciadocomercial from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_spro_procvaciadocomercial
end type
end forward

global type w_maed_spro_procvaciadocomercial from w_mant_encab_deta_csd
integer width = 3849
string title = "Confirmacion de Vaciado"
string menuname = ""
windowstate windowstate = maximized!
dw_3 dw_3
end type
global w_maed_spro_procvaciadocomercial w_maed_spro_procvaciadocomercial

type variables
uo_lineapacking 										iuo_LineaPacking	
uo_doctointernopack									iuo_doctointerno	
uo_plantadesp											iuo_plantalote
uo_clientesprod										iuo_cliente

datawindowchild       									idwc_planta, idwc_especies, idwc_linea, idwc_tipoenva, &
															idwc_envase, idwc_calidad, idwc_lineapacking, idwc_cliente

Datetime          										idt_fechasistema
str_envase												istr_Envase
str_mant													istr_mant2
w_mant_deta_spro_vaciadoproceso_bins_com	iw_detalles
end variables

forward prototypes
public subroutine buscacalidad (string as_calidad)
public function decimal buscalotesrepetidos (string as_lote, integer ai_secuen, integer ai_tipoen, integer ai_envase, long al_fila)
public subroutine buscaorden ()
public function boolean envasecosechero (integer ai_tipo)
public subroutine habilitaencab (boolean habilita)
public subroutine buscadetallelotes ()
public function boolean habilitabultos (string as_columna, string as_valor)
public function decimal buscabultos (string as_lote, integer ai_secuen, integer ai_tipoenva, integer ai_enva)
public function boolean duplicado (string as_columna, string as_valor)
public function decimal buscabultosyaingresados (string as_lote, integer ai_secuen, integer ai_tipoen, integer ai_envase)
public function boolean existeencabezado (long al_numero)
public subroutine actualizatotalbultos (string as_lote, integer ai_secuen, integer ai_tipoen, integer ai_envase, long al_fila, decimal ad_bultos)
public subroutine habilitadetalle (string as_columna)
public subroutine pesooriginalbins ()
public subroutine pesooriginalbultos ()
public subroutine pesobultoabulto ()
public subroutine eliminarbins (integer ai_row)
end prototypes

public subroutine buscacalidad (string as_calidad);String   ls_Nombre
Integer  li_tipoenva, li_codenva

li_tipoenva	=	istr_Envase.TipoEnvase
li_codenva	=	istr_Envase.codigo

SELECT	cale_nombre
	INTO	:ls_Nombre
	FROM	dbo.spro_calicosechero
  WHERE	enva_tipoen	=	:li_tipoenva
    AND  enva_codigo =  :li_codenva
	 AND  cale_calida =  :as_calidad;
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")

ELSEIF sqlca.SQLCode<>100 THEN
	dw_1.object.cale_nombre[il_fila]=ls_nombre
END IF
end subroutine

public function decimal buscalotesrepetidos (string as_lote, integer ai_secuen, integer ai_tipoen, integer ai_envase, long al_fila);Long  ll_fila
Decimal{2} ld_bultos
Integer li_secuen, li_tipoen, li_envase
String ls_lotefila

ld_bultos=0
FOR ll_fila=1 TO dw_1.RowCount()
   IF ll_fila<>al_fila THEN
		ls_lotefila=dw_1.Object.lote[ll_fila]
		li_secuen  =dw_1.Object.lfcd_secuen[ll_fila]
		li_tipoen  =dw_1.Object.enva_tipoen[ll_fila]
		li_envase  =dw_1.Object.enva_codigo[ll_fila]
		
		IF as_lote=ls_lotefila AND ai_tipoen=li_tipoen AND ai_envase=li_envase AND ai_secuen=li_secuen THEN
			ld_bultos = ld_bultos + dw_1.Object.opvd_canbul[ll_fila]
		END IF
		
	END IF
NEXT	
RETURN ld_bultos
end function

public subroutine buscaorden ();Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[2]
lstr_busq.argum[2]	=	istr_Mant.Argumento[7] 

OpenWithParm(w_busqueda_doctointernopack, lstr_busq)
lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	
	IF iuo_doctointerno.Existe(Integer(istr_mant.argumento[7]),	&
										Integer(lstr_busq.Argum[1]),&
										Integer(lstr_busq.Argum[2]), &
										Long(lstr_busq.argum[3]),True,Sqlca) THEN	
											
			istr_mant.argumento[3]	= lstr_busq.argum[3]
			istr_Mant.Argumento[4] = String(iuo_doctointerno.ld_Fecha)
			dw_2.SetItem(1,"orpr_numero",long(lstr_busq.argum[3]))
			
			IF existeencabezado(Long(istr_mant.argumento[3])) THEN
				TriggerEvent("ue_recuperadatos")
			ELSE
				dw_2.object.dinp_fecdoc[1] = iuo_doctointerno.ld_fecha
				dw_2.object.espe_codigo[1] = iuo_doctointerno.especie
				dw_2.Object.dinp_estado[1]	= iuo_doctointerno.estado
				
				Habilitadetalle("orpr_numero")
     		END IF	
	ELSE
		dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
	END IF
END IF
end subroutine

public function boolean envasecosechero (integer ai_tipo);Boolean	lb_Retorno=False
Integer  li_tipoenva

SELECT	tien_usoenv	INTO	:li_tipoenva
	FROM	dbo.tiposenvases
	WHERE	enva_tipoen	=	:ai_tipo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla tipos de envases")
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 and li_tipoenva=1 THEN

	lb_Retorno	=	True

END IF

RETURN lb_Retorno
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.opve_fecvac.Protect				=	0
	dw_2.Object.opve_fecvac.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.opve_turno.Protect				=	0
	dw_2.Object.opve_turno.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_tipord.Protect				=	0
	dw_2.Object.orpr_tipord.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.orpr_numero.Protect				=	0
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.line_codigo.Protect				=	0
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.b_orden.visible					=  1
ELSE
	dw_2.Object.opve_fecvac.Protect				=	1
	dw_2.Object.opve_fecvac.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.opve_turno.Protect				=	1
	dw_2.Object.opve_turno.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.orpr_tipord.Protect				=	1
	dw_2.Object.orpr_tipord.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.orpr_numero.Protect				=	1
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.line_codigo.Protect				=	1
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.b_orden.visible					=  0
END IF
end subroutine

public subroutine buscadetallelotes ();Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=  istr_mant.argumento[2]
lstr_busq.argum[3]	=  istr_mant.argumento[3]

OpenWithParm(w_busc_detalle_lotesvaciadoscomer, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("lote")
	dw_1.SetFocus()
ELSE
	
	dw_1.Object.lofc_pltcod[il_fila]	=	Integer(lstr_busq.argum[1])
	dw_1.Object.lofc_espcod[il_fila]	=	Integer(lstr_busq.argum[2])
	dw_1.Object.lofc_lotefc[il_fila]	=	Long(lstr_busq.argum[3])
	dw_1.Object.lote[il_fila]			=	(lstr_busq.argum[4])
	dw_1.Object.lfcd_secuen[il_fila]	=	integer(lstr_busq.argum[9])
	dw_1.Object.enva_tipoen[il_fila]	=	Integer(lstr_busq.argum[5])

	dw_1.GetChild("enva_codigo", idwc_envase)
	idwc_envase.SetTransObject(SqlCa)
	idwc_envase.Retrieve(integer(lstr_busq.argum[5]))
	
	dw_1.Object.enva_codigo[il_fila]	=	integer(lstr_busq.argum[6])
	dw_1.Object.enva_nombre[il_fila]	=	lstr_busq.argum[7]
	dw_1.Object.opvd_canbul[il_fila]	=	long(lstr_busq.argum[8])
	dw_1.Object.bultostot[il_fila]	=	Dec(lstr_busq.argum[8])
	
	ExisteEnvase(Integer(lstr_busq.argum[5]),Integer(lstr_busq.argum[6]) , istr_Envase)
	
	dw_1.GetChild("cale_calida", idwc_calidad)
	idwc_calidad.SetTransObject(SqlCa)
	idwc_calidad.Retrieve(Integer(lstr_busq.argum[5]),integer(lstr_busq.argum[6]))
	
	HabilitaBultos("lote",(lstr_busq.argum[4]))
	
	dw_1.Setcolumn("opvd_canbul")
	dw_1.SetFocus()
END IF

RETURN
end subroutine

public function boolean habilitabultos (string as_columna, string as_valor);Boolean	lb_Estado = True
decimal{2}     ld_Bultos, ld_bultosrep, ld_bultosyaingresados
String   ls_lote, ls_null
Integer  li_tipo, li_enva, li_secuen

SetNull(ls_Null)

IF as_Columna <> "lote" AND &
	(dw_1.Object.lote[il_fila] = "") or isnull(dw_1.Object.lote[il_fila]) THEN
	lb_Estado = False
END IF

IF as_Columna <> "lfcd_secuen" AND &
	(dw_1.Object.lfcd_secuen[il_fila] = 0) or isnull(dw_1.Object.lfcd_secuen[il_fila]) THEN
	lb_Estado = False
END IF

IF as_Columna <> "enva_tipoen" AND &
	(dw_1.Object.enva_tipoen[il_fila] = 0 OR IsNull(dw_1.Object.enva_tipoen[il_fila])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "enva_codigo" AND &
	(dw_1.Object.enva_codigo[il_fila] = 0 OR IsNull(dw_1.Object.enva_codigo[il_fila])) THEN
	lb_Estado = False
END IF

IF lb_estado THEN
	ls_lote	=	dw_1.Object.lote[il_fila]
	li_secuen=  dw_1.Object.lfcd_secuen[il_fila]
	li_tipo	=	dw_1.Object.enva_tipoen[il_fila]
	li_enva	=	dw_1.Object.enva_codigo[il_fila]
	
	IF as_columna="lote" 		 THEN ls_lote	=	as_valor
	IF as_columna="lfcd_secuen" THEN li_secuen=	integer(as_valor)
	IF as_columna="enva_tipoen" THEN li_tipo	=	integer(as_valor)
	IF as_columna="enva_codigo" THEN li_enva	=	integer(as_valor)
	
		IF ls_lote<>"" AND isnull(li_tipo)=FALSE AND isnull(li_enva)=FALSE THEN
		
		ld_Bultos=BuscaBultos(ls_lote,li_secuen,li_tipo, li_enva)
		
		IF ld_Bultos<= 0 THEN
			MessageBox("Atención","Número de Lote Ingresado No posee Bultos de Movimiento. Debe Ingresar Otro.")
			dw_1.Object.lote[il_fila]				=	ls_null
			dw_1.Object.lofc_lotefc[il_fila]    =  integer(ls_null)
			dw_1.Object.lfcd_secuen[il_fila]  	= 	Integer(ls_Null)
			dw_1.Object.enva_tipoen[il_fila]		=	Integer(ls_null)
			dw_1.Object.enva_codigo[il_fila]		=	Integer(ls_null)
			dw_1.Object.enva_nombre[il_fila]		=	ls_null
			dw_1.Object.cale_calida[il_fila]		=	ls_null
			dw_1.Object.cale_nombre[il_fila]		=	ls_null
			dw_1.Object.opvd_canbul[il_fila]		=	long(ls_null)
			dw_1.Object.bultostot[il_fila]		=	long(ls_null)
			
			RETURN FALSE
		ELSE	
			IF dw_1.RowCount()>1 THEN			  
				ld_bultosrep = buscalotesrepetidos(ls_lote,li_secuen,li_tipo,li_enva,il_fila)
			ELSE	 
				ld_bultosrep = 0
			END IF	 
			
		   ld_bultosyaingresados = buscabultosyaingresados(ls_lote,li_secuen,li_tipo,li_enva)
			
		   IF isnull(ld_bultosyaingresados) THEN ld_bultosyaingresados = 0
			
			ld_bultos = ld_bultos - ld_bultosyaingresados - ld_bultosrep
			IF ld_bultos<=0 THEN
				MessageBox("Atención","El lote ya fue ingresado en su totalidad. Debe Ingresar Otro.")
				dw_1.Object.lote[il_fila]				=	ls_null
				dw_1.Object.lofc_lotefc[il_fila]    =  integer(ls_null)
				dw_1.Object.lfcd_secuen[il_fila]		= 	Integer(ls_null)
				dw_1.Object.enva_tipoen[il_fila]		=	Integer(ls_null)
				dw_1.Object.enva_codigo[il_fila]		=	Integer(ls_null)
				dw_1.Object.enva_nombre[il_fila]		=	ls_null
				dw_1.Object.cale_calida[il_fila]		=	ls_null
				dw_1.Object.cale_nombre[il_fila]		=	ls_null
				dw_1.Object.opvd_canbul[il_fila]		=	long(ls_null)
				dw_1.Object.bultostot[il_fila]		=	long(ls_null)
				dw_1.Object.enva_codigo[il_fila]		=	Integer(ls_null)
				
				RETURN FALSE
				
			ELSE
				dw_1.Object.opvd_canbul[il_fila] 	=	ld_Bultos
				dw_1.Object.bultostot[il_fila]   	=	0  
				actualizatotalbultos(ls_lote,li_secuen,li_tipo, li_enva,il_fila,0)
			END IF
      END IF			

   END IF	
END IF	

RETURN TRUE
end function

public function decimal buscabultos (string as_lote, integer ai_secuen, integer ai_tipoenva, integer ai_enva);String   ls_Nombre, ls_envase, ls_codenva
Integer  li_tipodoc, li_orden, li_lotepl, li_lotesp, li_tipomov, li_cliente
Long     ll_numero,  ll_lote
Dec{2}   ld_Bultos

li_tipodoc	=	integer(istr_mant.argumento[2])
li_orden		=	integer(istr_mant.argumento[3])

li_lotepl   =	Integer(Mid(as_lote,1,4))
li_lotesp	=	Integer(mid(as_lote,5,2))
ll_lote		=	long(mid(as_lote,7,10))
li_cliente	=	Integer(istr_mant.argumento[7])

SELECT	distinct tpmv_codigo, mfco_numero
	INTO	:li_tipomov, :ll_numero
	FROM	dbo.spro_movtofrutacomenca
  WHERE	clie_codigo	=	:li_cliente
    AND 	plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
    AND 	tpmv_codigo =  25
    AND  mfco_tipdoc =  :li_Tipodoc
	 AND  mfco_docrel =  :li_orden; 

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Encabezado")

ELSEIF sqlca.SQLCode<>100 THEN
	
	 SELECT	smd.mfcd_bulent
		INTO	:ld_Bultos
		FROM	dbo.spro_movtofrutacomdeta smd, dbo.spro_lotesfrutacomdeta sld
  	  WHERE	smd.clie_codigo	=	:li_cliente
		 AND 	smd.plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
   	 AND  smd.tpmv_codigo	=  :li_tipomov
		 AND  smd.mfco_numero	=  :ll_numero
		 AND  smd.lofc_pltcod	=  :li_lotepl
		 AND  smd.lofc_espcod	=	:li_lotesp
		 AND  smd.lofc_lotefc	=	:ll_lote
		 AND  smd.lfcd_secuen	=  :ai_secuen
		 AND  sld.lofc_pltcod	=	smd.lofc_pltcod
		 AND  sld.lofc_espcod	=	smd.lofc_espcod
		 AND  sld.lofc_lotefc	=	smd.lofc_lotefc
		 AND  sld.lfcd_secuen	=	smd.lfcd_secuen
		 AND  sld.enva_tipoen	=	:ai_tipoenva
		 AND  sld.enva_codigo	=	:ai_enva;
   
	IF sqlca.SQLCode = -1 THEN
	   F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Detalle")
   ELSEIF sqlca.SQLCode<>100 THEN
		
   END IF

END IF

Return ld_Bultos
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
string   ls_loteco1, ls_tipoen1, ls_envase1, ls_planta1, ls_hora1, ls_secuen1
string   ls_loteco2, ls_tipoen2, ls_envase2, ls_planta2, ls_hora2, ls_secuen2

ls_loteco1 = string(dw_1.Object.lofc_lotefc[il_fila])
ls_secuen1 = string(dw_1.Object.lfcd_secuen[il_fila])
ls_planta1 = string(dw_1.Object.lofc_pltcod[il_fila])
ls_tipoen1 = string(dw_1.Object.enva_tipoen[il_fila])
ls_envase1 = string(dw_1.Object.enva_codigo[il_fila])
ls_hora1   = string(dw_1.Object.opvd_horava[il_fila],"hh:mm")

IF isnull(ls_Hora1) Or ls_Hora1 = "" THEN ls_Hora1 = "00:00"

CHOOSE CASE as_columna
	CASE "lofc_loteco"
		ls_loteco1 = as_valor
	
	CASE "lofc_pltcod"
		ls_planta1 = as_valor
		
	CASE "lfcd_secuen"
		ls_secuen1 = as_valor
		
	CASE "enva_tipoen"
		ls_tipoen1 = as_valor	
	
	CASE "enva_codigo"
		ls_envase1 = as_valor
		
	CASE "opvd_horava"
		ls_hora1 = MID(as_valor,12,5)	
		
END CHOOSE		

FOR ll_fila=1 To dw_1.Rowcount()
	IF ll_fila<>il_fila THEN
//      ls_loteco2 = string(dw_1.Object.lofc_lotefc[ll_fila])
//		ls_secuen2 = string(dw_1.Object.lfcd_secuen[ll_fila])
//		ls_planta2 = string(dw_1.Object.lofc_pltcod[ll_fila])
//		ls_tipoen2 = string(dw_1.Object.enva_tipoen[ll_fila])
//		ls_envase2 = string(dw_1.Object.enva_codigo[ll_fila])
		ls_hora2   = string(dw_1.Object.opvd_horava[ll_fila],"hh:mm")
   	IF isnull(ls_hora2) Or ls_Hora2="" THEN ls_Hora2 = "00:00"
		
//		IF ls_loteco1 = ls_loteco2 AND ls_secuen1 = ls_secuen2 AND ls_planta1 = ls_planta2 AND &
//		   ls_tipoen1 = ls_tipoen2 AND ls_envase1 = ls_envase2 AND ls_hora1   = ls_hora2 THEN
      IF ls_hora1   = ls_hora2 THEN
			MessageBox("Error", "La hora de vaciado para el lote ya fue ingresada anteriormente", Information!, Ok!)
			RETURN True
		END IF	
	END IF
NEXT	

RETURN False
end function

public function decimal buscabultosyaingresados (string as_lote, integer ai_secuen, integer ai_tipoen, integer ai_envase);Long ll_fila, ll_lote
decimal{2} ld_bultos = 0
Integer li_lotepl, li_lotesp, li_turno, li_turnofila
Date    ldt_fechava, ldt_fechafila

li_lotepl   =	Integer(Mid(as_lote,1,4))
li_lotesp	=	Integer(mid(as_lote,5,2))
ll_lote		=	Integer(mid(as_lote,7,10))
ldt_fechava =  dw_2.Object.opve_fecvac[1]
li_turno		=  dw_2.Object.opve_turno[1]

dw_3.SetFilter("lofc_pltcod = " + string(li_lotepl) + " AND " + &
               "lofc_espcod = " + string(li_lotesp) + " AND " + &
               "lofc_lotefc = " + string(ll_lote)   + " AND " + &
					"lfcd_secuen = " + string(ai_secuen) + " AND " + &
					"enva_tipoen = " + string(ai_tipoen) + " AND " + &
					"enva_codigo = " + string(ai_envase) )
dw_3.Filter()					

FOR ll_fila = 1 TO dw_3.RowCount()
	li_turnofila 	=	dw_3.Object.opve_turno[ll_fila]
	ldt_fechafila 	=	dw_3.Object.opve_fecvac[ll_fila]
	IF li_turnofila <> li_turno OR ldt_fechafila  <> ldt_fechava THEN
		
		ld_bultos = ld_bultos + dw_3.Object.opvd_canbul[ll_fila]
		
	END IF	 
NEXT	

IF isnull(ld_bultos) THEN ld_Bultos = 0

dw_3.SetFilter("")
dw_3.Filter()

RETURN ld_bultos

end function

public function boolean existeencabezado (long al_numero);Boolean	lb_Retorno
Integer	li_Planta, li_tpmv, li_Cantidad, li_turno, li_cliente
Date		ldt_fecha

ldt_Fecha		=	Date(istr_Mant.Argumento[4])
li_tpmv			=	Integer(istr_Mant.Argumento[2])
li_turno       =  Integer(istr_Mant.Argumento[5])
li_cliente		=	Integer(istr_mant.argumento[7])

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dbo.spro_ordprocvaccomenca
	WHERE	clie_codigo	=	:li_cliente
	AND 	plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
	AND   orpr_tipord	=	:li_tpmv
	AND	orpr_numero	=	:al_numero
	AND 	opve_fecvac	=	:ldt_fecha
	AND 	opve_turno	=	:li_turno;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvaenca")
	
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 and li_cantidad>0 THEN

	lb_Retorno	=	True

END IF

RETURN lb_Retorno
end function

public subroutine actualizatotalbultos (string as_lote, integer ai_secuen, integer ai_tipoen, integer ai_envase, long al_fila, decimal ad_bultos);Long    ll_fila
Integer li_tipoen, li_envase, li_secuen
String  ls_lotefila

FOR ll_fila=1 TO dw_1.RowCount()
   IF ll_fila<>al_fila THEN
		
		ls_lotefila= dw_1.Object.lote[ll_fila]
		li_tipoen  = dw_1.Object.enva_tipoen[ll_fila]
		li_envase  = dw_1.Object.enva_codigo[ll_fila]
		li_secuen  = dw_1.Object.lfcd_secuen[ll_fila]
		
		IF as_lote = ls_lotefila AND ai_tipoen = li_tipoen AND ai_envase = li_envase AND &
		   ai_secuen = li_secuen THEN
			
			dw_1.Object.bultostot[ll_fila] = ad_bultos
		END IF
		
	END IF
NEXT	
end subroutine

public subroutine habilitadetalle (string as_columna);Boolean	lb_Estado = True
Date		ldt_fecha
Integer  li_planta, li_tipord 
Long     ll_orden

IF as_Columna <> "opve_fecvac" AND &
	(dw_2.Object.opve_fecvac[1] = ldt_fecha) THEN
	lb_Estado = False
END IF

IF as_Columna <> "orpr_numero" AND &
	(dw_2.Object.orpr_numero[1] = 0 OR IsNull(dw_2.Object.orpr_numero[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "opve_turno" AND &
	(dw_2.Object.opve_turno[1] = 0 OR IsNull(dw_2.Object.opve_turno[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "line_codigo" AND &
	(dw_2.Object.line_codigo[1] = 0 OR IsNull(dw_2.Object.line_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "opve_estado" AND &
	(dw_2.Object.opve_estado[1] = "C" OR IsNull(dw_2.Object.opve_estado[1])) THEN
	lb_Estado = False
END IF

IF lb_estado THEN
  pb_ins_det.Enabled = lb_estado
  li_planta 	=	dw_2.Object.plde_codigo[1]
  li_tipord 	=	dw_2.Object.orpr_tipord[1]
  ll_orden		=	dw_2.Object.orpr_numero[1]
  dw_3.SetTransObject(SQLCA)
  dw_3.Retrieve(dw_2.Object.clie_codigo[1], li_planta,li_tipord, ll_orden)
  
END IF	

end subroutine

public subroutine pesooriginalbins ();Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote
Integer	li_TipEn, li_CodEn, bultos
String		ls_Calid
Decimal	ldec_KilOri, ldec_Tara

ll_fila 			=	dw_1.GetRow() 

IF ll_fila < 1 THEN RETURN

ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
bultos		=	dw_1.Object.opvd_canbul[ll_fila]
li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
ll_lote		=	dw_1.Object.lofc_lotefc[ll_fila]

SELECT Sum(mfgp.lfcd_kilnet)
  INTO :ldec_KilOri
  FROM dbo.spro_lotesfrutacomdeta as mfgp
 WHERE mfgp.fgmb_nrotar in (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
	AND lofc_lotefc = :ll_lote;

IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_LotesFrutaComDeta")
ELSE
	
//	SELECT	cale_pesoen
//	INTO	:ldec_Tara
//	FROM	dbo.spro_calicosechero
//	WHERE  enva_tipoen	=	:li_TipEn
//		AND  enva_codigo =  :li_CodEn
//		AND  cale_calida =  :ls_Calid;
//	 
//	IF sqlca.SQLCode <> 0 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
//	ELSE
		dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri /*- (ldec_tara * Bultos)*/, 2)
//	END IF
	
END IF


end subroutine

public subroutine pesooriginalbultos ();Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote, ll_tarja
Integer	li_TipEn, li_CodEn, li_bultos, li_Cliente, li_Planta, li_Numero, li_especie, li_tipoenva, li_envase
String		ls_Calid, ls_calidad
Decimal	ldec_KilOri, ldec_Tara,ld_tarabasepallet
	
ll_fila 			=	il_Fila

IF ll_fila < 1 THEN RETURN

ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
li_bultos		=	dw_1.Object.opvd_canbul[ll_fila]
li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
ll_lote			=	dw_1.Object.lote_codigo[ll_fila]

SELECT	Sum(mfgp.lfcd_kilnet)
	INTO	:ldec_KilOri
	FROM	dbo.spro_lotesfrutacomdeta as mfgp
	WHERE 	mfgp.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
	    AND	lofc_lotefc = :ll_lote;

IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_lotesfrutacomdeta")
ELSE
	dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
END IF

end subroutine

public subroutine pesobultoabulto ();Integer	li_filas, li_planta, li_especie, li_TipEnv, li_CodEnv
Long		ll_lote, ll_bultos
decimal	ldec_kilos_prom_originales, ldec_kilos_tara
String		ls_calidad

FOR li_filas = 1 to dw_1.RowCount()
	li_planta	=	dw_1.Object.lote_pltcod[li_filas]
	li_especie	=	dw_1.Object.lote_espcod[li_filas]
	ll_lote		=	dw_1.Object.lote_codigo[li_filas]
	li_TipEnv	=	dw_1.Object.enva_tipoen[li_filas]
	li_CodEnv	=	dw_1.Object.enva_codigo[li_filas]
	ls_calidad	=	dw_1.Object.cale_calida[li_filas]
	ll_bultos	=	dw_1.Object.opvd_canbul[li_filas]
	
	dw_1.Object.opvd_pesone[li_filas] 	=	0	//peso neto vaciado
	dw_1.Object.opvd_pesobr[li_filas] 	=	0 	//peso bruto vaciado
	dw_1.Object.opvd_kilpro[li_filas] 	=	0	//peso promedio neto
	dw_1.Object.opvd_kilori[li_filas] 	=	0	//peso neto original	
	
	SELECT lofc_totkil / lofc_totbul INTO :ldec_kilos_prom_originales
		FROM dbo.spro_lotesfrutacomenc
		WHERE lofc_pltcod		=	:li_planta
			AND lofc_espcod	=	:li_especie
			AND lofc_lotefc		=	:ll_lote;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
	ELSE
			//peso neto vaciado
			dw_1.Object.opvd_pesone[li_filas] 	=	ldec_kilos_prom_originales * ll_bultos 
			//peso bruto vaciado
			dw_1.Object.opvd_pesobr[li_filas] 	= 	( ldec_kilos_prom_originales * ll_bultos ) + ( ldec_kilos_tara * ll_bultos )
			//peso promedio neto
			dw_1.Object.opvd_kilpro[li_filas]		=	ldec_kilos_prom_originales
			//peso neto original
			dw_1.Object.opvd_kilori[li_filas]		=	ldec_kilos_prom_originales * ll_bultos
	END IF
	
NEXT
end subroutine

public subroutine eliminarbins (integer ai_row);Integer li_i

//FOR li_i 	=	1 TO dw_4.RowCount()
//	
//	IF dw_4.Object.opva_secuen[li_i] = ai_row THEN
//		dw_4.DeleteRow(li_i)
//		li_i = li_i - 1
//	ELSEIF dw_4.Object.opva_secuen[li_i] > ai_row THEN
//		dw_4.Object.opva_secuen[li_i] = dw_4.Object.opva_secuen[li_i] - 1
//	END IF
//	
//NEXT
end subroutine

on w_maed_spro_procvaciadocomercial.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_spro_procvaciadocomercial.destroy
call super::destroy
destroy(this.dw_3)
end on

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo Orden
istr_Mant.Argumento[3]	=	Numero
istr_Mant.Argumento[4]	=	Fecha Vaciado
istr_Mant.Argumento[5]	=	Turno
istr_Mant.Argumento[6]	=	Estado
istr_Mant.Argumento[7]	=	Cliente
*/

x												= 	0
y												= 	0

This.Height									= 	2520
im_menu										= 	m_principal

This.ParentWindow().ToolBarVisible	= 	True
im_menu.Item[1].Item[6].Enabled		= 	True
im_menu.Item[7].Visible					= 	True

//IF gstr_paramplanta.binsabins OR gstr_paramplanta.palletdebins OR gstr_paramplanta.bultobins THEN
//	dw_1.DataObject = "dw_mant_mues_procvaccomdeta_idinv"
//ELSE
	dw_1.DataObject = "dw_mant_mues_procvaccomdeta_correc"
//	dw_1.DataObject = "dw_mant_mues_procvaccomdeta"
//END IF

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"5"
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	""
istr_Mant.Argumento[5]	=	""
istr_Mant.Argumento[6]	=	"V"
istr_Mant.Argumento[7]	=	String(gstr_parempresa.empr_codexp)


dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()

dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve(gstr_parempresa.empr_codexp)

dw_2.GetChild("line_codigo", idwc_linea)
idwc_linea.SetTransObject(sqlca)
idwc_linea.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.getChild("line_codigo", idwc_lineapacking)
idwc_lineapacking.SetTransObject(SQLCA)
idwc_lineapacking.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("cale_calida", idwc_calidad)
idwc_calidad.SetTransObject(SqlCa)
idwc_calidad.InsertRow(0)

dw_1.GetChild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(SqlCa)
idwc_envase.InsertRow(0)

dw_1.GetChild("enva_tipoen", idwc_tipoenva)
idwc_tipoenva.SetTransObject(SqlCa)
idwc_tipoenva.Retrieve()
idwc_tipoenva.SetFilter("tien_usoenv = 1")
idwc_tipoenva.Filter()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

iuo_LineaPacking			=	Create uo_lineapacking
iuo_doctointerno			=	Create uo_doctointernopack
iuo_plantalote				=	Create uo_plantadesp				
iuo_cliente					=	Create uo_clientesprod

pb_nuevo.PostEvent(Clicked!)
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif1+=	dw_2.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 ANd ll_Modif1>0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.SetRedraw(True)

dw_2.getChild("line_codigo", idwc_lineapacking)
idwc_lineapacking.SetTransObject(SQLCA)
idwc_lineapacking.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.InsertRow(0)
dw_2.SetFocus()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

idt_fechasistema 			= 	F_fechahora()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.clie_codigo[1]	=	Integer(istr_mant.Argumento[7])
dw_2.Object.orpr_tipord[1]	=	5
dw_2.Object.opve_fecvac[1]	=	Date(String(Today(),'dd/mm/yyyy'))
dw_2.Object.opve_estado[1] =  "V"

istr_mant.argumento[2] 		=	"5"
istr_mant.argumento[3]		=	""
istr_mant.argumento[4]		=	String(Today(),'dd/mm/yyyy')
istr_mant.argumento[6]		=	"V"

habilitaencab(TRUE)
dw_2.SetColumn("opve_turno")




end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row
Date		ldt_fecha
Integer  li_Protec=0, li_secuen
String   ls_lote
Decimal{2} ld_bultos, ld_bultosrep, ld_bultosyaingresados

ldt_Fecha	=	Date(istr_Mant.Argumento[4])

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[7]), &
										  Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]), &
										  ldt_fecha, integer(istr_mant.Argumento[5]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		DO
			IF dw_2.RowCount() > 0 THEN
				dw_3.SetTransObject(SQLCA)
		 		dw_3.Retrieve(Integer(istr_Mant.Argumento[7]), &
								  Integer(istr_mant.Argumento[1]), &
								  Integer(istr_mant.Argumento[2]), &
                          Integer(istr_mant.Argumento[3]))
			END IF	


			ll_fila_d	=	dw_1.Retrieve(Integer(istr_Mant.Argumento[7]), &
										  		  Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
										  			ldt_fecha, integer(istr_mant.Argumento[5]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				
				IF dw_1.RowCount()>0 THEN
					IF dw_2.Object.orpr_estado[1]	=	3 THEN 
						li_Protec = 1
					END IF
					
					IF NOT gstr_paramplanta.binsabins AND &
						NOT gstr_paramplanta.palletdebins AND &
						NOT gstr_paramplanta.bultobins THEN
						
						FOR ll_row = 1 TO dw_1.RowCount()
							ls_lote	=	String(dw_1.Object.lofc_pltcod[ll_row],'0000') + &
											String(dw_1.Object.lofc_espcod[ll_row],'00') + &
											String(dw_1.Object.lofc_lotefc[ll_row],'00000000')
										 
							dw_1.Object.lote[ll_row]		=	ls_lote
							li_secuen 							=  dw_1.Object.lfcd_secuen[ll_row]
						 
							ld_bultos=BuscaBultos(ls_lote,li_secuen,dw_1.Object.enva_tipoen[ll_row],&
													  dw_1.Object.enva_codigo[ll_row])
									  
							IF dw_1.RowCount()>1 THEN			  
								ld_bultosrep = buscalotesrepetidos(ls_lote,li_secuen,dw_1.Object.enva_tipoen[ll_row],&
																			  dw_1.Object.enva_codigo[ll_row], ll_row)
							ELSE	 
								ld_bultosrep = 0
							END IF
							
							ld_bultosyaingresados = buscabultosyaingresados(ls_lote,li_secuen,dw_1.Object.enva_tipoen[ll_row],&
																						 dw_1.Object.enva_codigo[ll_row])
				
							IF isnull(ld_bultosyaingresados) THEN ld_bultosyaingresados = 0
							
							ld_bultos 							= ld_bultos - ld_bultosyaingresados - ld_bultosrep - dw_1.Object.opvd_canbul[ll_row]
							dw_1.Object.bultostot[ll_row] = ld_bultos
							dw_1.Object.protec[ll_row] 	= li_Protec
							dw_1.SetItemStatus(ll_row, 0, Primary!, NotModified!)
						 
						NEXT
					END IF
					
					dw_1.GetChild("enva_codigo", idwc_envase)
					idwc_envase.SetTransObject(SqlCa)
					idwc_envase.Retrieve(dw_1.Object.enva_tipoen[1])
					
					dw_1.GetChild("cale_calida", idwc_calidad)
					idwc_calidad.SetTransObject(SqlCa)
					idwc_calidad.Retrieve(dw_1.Object.enva_tipoen[1],dw_1.Object.enva_codigo[1])
					
				END IF
				IF dw_2.Object.opve_estado[1] = "C" THEN
					pb_eli_det.Enabled	=	FALSE
					pb_ins_det.Enabled   =  FALSE
					pb_grabar.Enabled		=	FALSE

					IF NOT gstr_paramplanta.binsabins AND &
						NOT gstr_paramplanta.palletdebins AND &
						NOT gstr_paramplanta.bultobins THEN
						dw_1.Object.b_lotes.visible = 0
					END IF
				ELSE
					pb_ins_det.Enabled   =  TRUE
					pb_grabar.Enabled		=	TRUE
					pb_eliminar.Enabled	=	TRUE
					
					IF NOT gstr_paramplanta.binsabins AND &
						NOT gstr_paramplanta.palletdebins AND &
						NOT gstr_paramplanta.bultobins THEN
						dw_1.Object.b_lotes.visible = 1
					END IF
					
					IF dw_1.RowCount() > 1 THEN
					   pb_eli_det.Enabled	=	TRUE
					ELSE
						pb_eli_det.Enabled	=	FALSE
					END IF	
				END IF	
							
				pb_imprimir.Enabled		=	TRUE
			
				HabilitaEncab(False)
				pb_ins_det.SetFocus()
				
			END IF
			dw_2.SetRedraw(True)
			
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

event ue_nuevo_detalle;IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
	pb_eli_det.Enabled = True
END IF

IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.BultoBins THEN
	il_fila = dw_1.InsertRow(0)
	
	dw_1.Object.lofc_pltcod[il_fila] =  Integer(istr_mant.Argumento[1])
	dw_1.Object.lofc_espcod[il_fila] =  dw_2.Object.espe_codigo[1]
	//dw_1.Object.opvd_horava[il_Fila]	=	dw_2.Object.opve_fecvac[1]
	//dw_1.Object.opvd_horate[il_Fila]	=	dw_2.Object.opve_fecvac[1]
	
	dw_1.SetItemStatus(il_fila,0,Primary!,NotModified!)
	
	habilitaencab(False)
	
	dw_1.SetRow(il_fila)
	dw_1.SetColumn(1)
	dw_1.SetFocus()
	
ELSE
	Istr_mant2.dw 					= 	dw_1
	istr_mant2.Argumento[1]		= 	String(dw_2.Object.clie_codigo[1])
	istr_mant2.Argumento[2]		= 	String(dw_2.Object.plde_codigo[1])
	istr_mant2.Argumento[3]		= 	String(dw_2.Object.orpr_tipord[1])
	istr_mant2.Argumento[4]		= 	String(dw_2.Object.orpr_numero[1])
	istr_mant2.Argumento[5]		= 	String(dw_2.Object.espe_codigo[1])
	istr_mant2.Argumento[6]		=	String(dw_1.RowCount())
	istr_mant2.Argumento[7]		=	String(dw_2.Object.opve_fecvac[1],"dd/mm/yyyy")
	istr_mant2.Argumento[8]		=	"0"
	istr_mant2.Argumento[9]		=	String(il_fila)
	istr_mant2.Argumento[10]	=	"0"//String(dw_4.RowCount())
	istr_mant2.agrega				= 	True
	OpenWithParm(iw_detalles, istr_mant2)
	PesoOriginalBins()
END IF
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_detalles, istr_mant)
//END IF
end event

event ue_borra_detalle;call super::ue_borra_detalle;Long ll_bultos
String ls_lote
Integer  li_tipen, li_envas, li_secuen

IF istr_Mant.Solo_Consulta THEN RETURN

IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	ll_bultos = dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
	ls_lote   = dw_1.Object.lote[il_fila]
	li_tipen  = dw_1.Object.enva_tipoen[il_fila]
	li_envas  = dw_1.Object.enva_codigo[il_fila]
	IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.bultobins THEN
		li_secuen = dw_1.Object.lfcd_secuen[il_fila]
	END IF
	
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.bultobins THEN
			actualizatotalbultos(ls_lote,li_secuen,li_tipen, li_envas,0,ll_bultos)
		END IF
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_antesguardar;Long ll_fila, ll_row, ll_paso, suma, bultostotal
Integer li_lotepl, li_lotesp, li_lote, li_contador
Boolean lb_pasada=True
String	ls_Mensaje, ls_Columna[], ls_fecha, ls_hora

Message.DoubleParm = 1

IF dw_1.RowCount()<1 THEN
	MessageBox("Error de Detalle", "No ha Ingresado Detalle.")
	Message.DoubleParm = -1
	RETURN
END IF

FOR ll_fila=1 TO dw_1.RowCount()
	IF IsNull(dw_1.Object.cale_calida[ll_fila]) OR dw_1.Object.cale_calida[ll_fila] = "" THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nCalidad Envase"
		ls_Columna[li_Contador]	=	"cale_calida"
	END IF
	
	IF IsNull(dw_1.Object.enva_tipoen[ll_fila]) OR dw_1.Object.enva_tipoen[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nTipo de Envase"
		ls_Columna[li_Contador]	=	"enva_tipoen"
	END IF
	
	IF IsNull(dw_1.Object.enva_codigo[ll_fila]) OR dw_1.Object.enva_codigo[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nEnvase"
		ls_Columna[li_Contador]	=	"enva_codigo"
	END IF
	
	IF IsNull(dw_1.Object.opvd_canbul[ll_fila]) OR dw_1.Object.opvd_canbul[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nBultos"
		ls_Columna[li_Contador]	=	"opvd_canbul"
	END IF
	
	IF li_Contador > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_Columna[1])
		dw_1.SetRow(ll_fila)
		dw_1.SetFocus()
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF dw_1.Object.opvd_horava[ll_fila] > dw_1.Object.opvd_horate[ll_fila] THEN
		MessageBox("Error de Detalle", "En la Fila " + String(ll_fila) + " La hora de inicio no puede ser mayor a la hora de término.")
		Message.DoubleParm = -1
		RETURN
	END IF	

	IF dw_1.Object.opvd_canbul[ll_fila] < 0  OR dw_1.Object.opvd_canbul[ll_fila] > 9999.99 THEN
		MessageBox("Error de Detalle", "En la Fila " + String(ll_fila) + " la cantidad de bultos debe ser mayor a cero y menor a 9.999,99")
		Message.DoubleParm = -1
		RETURN
	END IF
NEXT
	
FOR ll_Fila=1 To dw_1.Rowcount()
	IF dw_1.Object.opvd_canbul[ll_fila]=0 or isnull(dw_1.Object.opvd_canbul[ll_fila]) OR &
		/*dw_1.Object.lote[ll_fila]="" or isnull(dw_1.Object.lote[ll_fila])Or */isnull(dw_1.Object.enva_tipoen[ll_fila]) OR &
		isnull(dw_1.Object.enva_codigo[ll_fila]) THEN
		MessageBox("Error de Detalle", "En la Fila " + String(ll_fila) + " No ha ingresado Datos.")
		Message.DoubleParm = -1
		ll_fila=dw_1.rowcount()
		RETURN
	ELSE
//		BultosTotal=dw_1.Object.BultosTot[ll_fila]
//		suma=dw_1.Object.opvd_canbul[ll_fila]
//		For ll_row=(ll_fila+1) to dw_1.rowcount()
//			IF ((dw_1.Object.lote[ll_fila]=dw_1.Object.lote[ll_row]) AND &
//				(dw_1.Object.enva_tipoen[ll_fila]=dw_1.Object.enva_tipoen[ll_row]) AND &
//				(dw_1.Object.enva_codigo[ll_fila]=dw_1.Object.enva_codigo[ll_row])) THEN
//				suma=suma+dw_1.Object.opvd_canbul[ll_row]
//				IF Suma>BultosTotal THEN
//					MessageBox("Error Total Bultos","Bultos del Lote " + string(dw_1.Object.lote[ll_fila]) + " ~r Envase " + string(dw_1.Object.enva_nombre[ll_fila]) + " Sobrepasan a los Bultos del Movimiento.",Exclamation!)
//					Message.DoubleParm = -1
//					ll_fila=dw_1.rowcount()
//					ll_row=dw_1.rowcount()
//					RETURN
//				END IF
//			END IF	
//		NEXT	
	END IF	
NEXT	
	
FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.clie_codigo[ll_Fila]	=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.orpr_tipord[ll_Fila]	=	dw_2.Object.orpr_tipord[1]
		dw_1.Object.orpr_numero[ll_Fila]	=	dw_2.Object.orpr_numero[1]
		dw_1.Object.opve_turno[ll_Fila]	=	dw_2.Object.opve_turno[1]
		dw_1.Object.opve_fecvac[ll_Fila]	=	dw_2.Object.opve_fecvac[1] 

		ls_fecha = String(dw_2.Object.opve_fecvac[1],"dd/mm/yyyy")
		ls_hora  = String(dw_1.Object.opvd_horava[ll_Fila],"hh:mm:ss")
		dw_1.Object.opvd_horava[ll_Fila]	=	Time(ls_hora)
		ls_hora  = String(dw_1.Object.opvd_horate[ll_Fila],"hh:mm:ss")
      dw_1.Object.opvd_horate[ll_Fila]	=	Time(ls_hora)
		
	END IF
NEXT
	


end event

event ue_seleccion;call super::ue_seleccion;	Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=	istr_mant.argumento[2]
lstr_busq.argum[3]	=	istr_mant.argumento[3]
lstr_busq.argum[4]	=	""
lstr_busq.argum[7]	=	istr_mant.argumento[7]

OpenWithParm(w_busc_spro_ordenvaciado, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
	dw_2.Object.plde_codigo[1] = integer(lstr_busq.argum[1])
	dw_2.Object.orpr_tipord[1] = integer(lstr_busq.argum[2])
	dw_2.Object.opve_fecvac[1] = Date(lstr_busq.argum[4])
	dw_2.Object.opve_turno[1]  = integer(lstr_busq.argum[5])
	
	IF iuo_doctointerno.Existe(Integer(istr_mant.argumento[7]),	&
										integer(lstr_busq.argum[1]),&
										Integer(lstr_busq.argum[2]), &
										Long(lstr_busq.argum[3]),True,Sqlca) THEN	
										
			istr_mant.argumento[1]	= lstr_busq.argum[1]
			istr_mant.argumento[2]	= lstr_busq.argum[2]
			istr_mant.argumento[3]	= lstr_busq.argum[3]
			istr_mant.argumento[4]	= lstr_busq.argum[4]
			istr_mant.argumento[5]	= lstr_busq.argum[5]
			
			IF existeencabezado(Long(istr_mant.argumento[3])) THEN
				TriggerEvent("ue_recuperadatos")
			ELSE
				dw_2.object.orpr_fecpro[1] = iuo_doctointerno.ld_fecha
				dw_2.object.espe_codigo[1] = iuo_doctointerno.especie
			END IF	
			
		ELSE
			dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
		END IF
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date		ldt_fecha

istr_info.titulo	= "CONFIRMACION DE VACIADO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_procvaccomenca"
vinf.dw_1.SetTransObject(sqlca)
ldt_Fecha	=	Date(istr_Mant.Argumento[4])
fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
								  Integer(istr_mant.Argumento[2]), &
								  Integer(istr_mant.Argumento[3]), &
								  ldt_fecha, integer(istr_mant.Argumento[5]), &
								  Integer(istr_mant.Argumento[7]))

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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_procvaciadocomercial
string tag = "dw_mant_mues_procvaccomdeta_idinv"
integer x = 50
integer y = 468
integer width = 3077
integer height = 1248
string title = "Detalle de Proceso Vaciado"
string dataobject = "dw_mant_mues_procvaccomdeta_correc"
end type

event dw_1::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	dw_1.GetChild("enva_codigo", idwc_envase)
	idwc_envase.SetTransObject(SqlCa)
	idwc_envase.Retrieve(this.Object.enva_tipoen[row])
END IF

RETURN 0
end event

event dw_1::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::rowfocuschanged;//ib_datos_ok = True
//
//IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
//	ib_datos_ok = False
//ELSE
//	il_fila = CurrentRow
//END IF
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

event dw_1::itemchanged;String     ls_Columna, ls_Nula, ls_lote
datetime   ldt_fechahora
decimal{2} ld_bultos, ld_total
Long       ll_lote
Integer    li_tipen, li_envas, li_secuen


ldt_fechahora=datetime(Date(istr_mant.argumento[4]),Time('00:00'))

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "enva_tipoen"
		ld_bultos = dw_1.Object.opvd_canbul[row] + dw_1.Object.bultostot[row]
		ls_lote   = dw_1.Object.lote[row]
		li_secuen = dw_1.Object.lfcd_secuen[row]
		li_tipen  = dw_1.Object.enva_tipoen[row]
		li_envas  = dw_1.Object.enva_codigo[row]	
		IF NOT ExisteEnvase(Integer(Data), 0, istr_Envase) OR Duplicado("enva_tipoen", Data)THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			IF istr_envase.UsoEnvase <> 1 THEN
				messagebox("Atención","El tipo de envase ingresado no es de uso cosechero. Ingrese o seleccione otro.")
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
			   This.SetItem(row, "enva_nombre", ls_Nula)
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			END IF	
			dw_1.GetChild("enva_codigo", idwc_envase)
			idwc_envase.SetTransObject(SqlCa)
			idwc_envase.Retrieve(integer(data))
			actualizatotalbultos(ls_lote,li_secuen,li_tipen, li_envas,0,ld_bultos)
			
			IF NOT HabilitaBultos("enva_tipoen",data) THEN
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
			   This.SetItem(row, "enva_nombre", ls_Nula)
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			END IF	
		END IF


	CASE "enva_codigo"
		ld_bultos = dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote   = dw_1.Object.lote[il_fila]
		li_secuen = dw_1.Object.lfcd_secuen[il_fila]
		li_tipen  = dw_1.Object.enva_tipoen[il_fila]
		li_envas  = dw_1.Object.enva_codigo[il_fila]	
		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR Duplicado("enva_codigo", Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			This.SetItem(row, "enva_nombre", ls_Nula)
			RETURN 1
		ELSE
			dw_1.GetChild("cale_calida", idwc_calidad)
			idwc_calidad.SetTransObject(SqlCa)
			idwc_calidad.Retrieve(istr_Envase.TipoEnvase,integer(data))
			This.Object.enva_nombre[row]	=	istr_Envase.Nombre
 		   actualizatotalbultos(ls_lote,li_secuen,li_tipen, li_envas,0,ld_bultos)
			IF NOT HabilitaBultos("enva_codigo",data) THEN
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
			   This.SetItem(row, "enva_nombre", ls_Nula)
				RETURN 1
			END IF	
		END IF
	
	
	CASE "opvd_horava"
		 IF Duplicado(ls_columna, Data) THEN
			This.SetItem(row, ls_Columna, ldt_fechahora)
			RETURN 1
		END IF
		
	CASE "opvd_canbul"
		ld_bultos = dw_1.Object.opvd_canbul[il_fila]
		IF dwo.Type = 'column' THEN
			IF Not This.uf_validate(row) THEN
				This.SetItem(row,"opvd_canbul",Integer(ls_Nula))
				RETURN 1
			END IF
		END IF
		ld_total =  dw_1.Object.bultostot[row] + ld_bultos
		IF (Long(Data) > ld_total) THEN 
			MessageBox("Atención","Número de Bultos Ingresado Sobrepasa a los Bultos del Movimiento.")
			This.SetItem(row, ls_Columna, ld_bultos)
			RETURN 1
		ELSE
			dw_1.Object.bultostot[row] = ld_total - Long(Data)
			actualizatotalbultos(dw_1.Object.lote[il_fila],dw_1.Object.lfcd_secuen[il_fila],dw_1.Object.enva_tipoen[il_fila], &
			                     dw_1.Object.enva_codigo[il_fila],il_fila,dw_1.Object.bultostot[row])
		END IF
	
   CASE "cale_calida"
		BuscaCalidad(data)
		
	CASE "lofc_lotefc"
		ld_bultos = dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote   = dw_1.Object.lote[il_fila]
		li_secuen = dw_1.Object.lfcd_secuen[il_fila]
		li_tipen = dw_1.Object.enva_tipoen[il_fila]
		li_envas = dw_1.Object.enva_codigo[il_fila]	
		IF isnull(dw_1.Object.lofc_pltcod[row]) OR dw_1.Object.lofc_pltcod[row]=0 THEN
			MessageBox("Atención","Primero ingrese una planta para el lote.")
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
		IF Duplicado("lofc_lotefc", Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		ll_Lote = Integer(data)
		dw_1.Object.lote[row] = String(dw_1.Object.lofc_pltcod[row],'0000') + &
										String(dw_1.Object.lofc_espcod[row],'00') + &
										String(ll_Lote,'00000000')
										
		actualizatotalbultos(ls_lote,li_secuen,li_tipen, li_envas,0,ld_bultos)
		
		IF NOT HabilitaBultos("lote",dw_1.Object.lote[row]) THEN
			This.SetItem(row, ls_Columna, ls_Nula)
			RETURN 1
		END IF	
	
	CASE "lfcd_secuen"
		ld_bultos = dw_1.Object.opvd_canbul[row] + dw_1.Object.bultostot[row]
		ls_lote   = dw_1.Object.lote[row]
		li_secuen = dw_1.Object.lfcd_secuen[row]
		li_tipen  = dw_1.Object.enva_tipoen[row]
		li_envas  = dw_1.Object.enva_codigo[row]	
		
		IF isnull(dw_1.Object.lofc_pltcod[row]) OR dw_1.Object.lofc_pltcod[row]=0 THEN
			MessageBox("Atención","Primero ingrese una planta para el lote.")
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
		IF isnull(dw_1.Object.lofc_lotefc[row]) OR dw_1.Object.lofc_lotefc[row]=0 THEN
			MessageBox("Atención","Primero ingrese un lote.")
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
		IF Duplicado("lfcd_secuen", Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
	
		actualizatotalbultos(ls_lote,li_secuen,li_tipen, li_envas,0,ld_bultos)
		
		IF NOT HabilitaBultos("lfcd_secuen",data) THEN
			This.SetItem(row, ls_Columna, ls_Nula)
			RETURN 1
		END IF		
			
	CASE "lofc_pltcod"
		ld_bultos = dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote   = dw_1.Object.lote[il_fila]
		li_secuen = dw_1.Object.lfcd_secuen[il_fila]
		li_tipen  = dw_1.Object.enva_tipoen[il_fila]
		li_envas  = dw_1.Object.enva_codigo[il_fila]	
		
		actualizatotalbultos(ls_lote,li_secuen,li_tipen, li_envas,0,ld_bultos)
		
		This.SetItem(row,"lote" , ls_Nula)
		This.SetItem(row,"lofc_lotefc",integer(ls_Nula))
		This.SetItem(row,"enva_tipoen",integer(ls_Nula))
		This.SetItem(row,"enva_codigo",integer(ls_Nula))
		This.SetItem(row,"enva_nombre" , ls_Nula)
		This.SetItem(row,"opvd_canbul" ,long(ls_Nula))
		This.SetItem(row,"bultostot" , long(ls_Nula))
		
		IF Not iuo_plantalote.existe(integer(data),True,Sqlca) OR Duplicado("lofc_pltcod", Data)THEN
			This.SetItem(row,"lote" , ls_Nula)
			this.SetItem(row,"lofc_lotefc",integer(ls_Nula))
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
END CHOOSE
end event

event type long dw_1::dwnkey(keycode key, unsignedlong keyflags);il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_spro_procvaciadocomercial.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_spro_procvaciadocomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event dw_1::getfocus;//
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name	
	CASE "b_lotes"
       BuscaDetalleLotes()
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;return 1
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_procvaciadocomercial
integer x = 137
integer y = 56
integer width = 2802
integer height = 320
string dataobject = "dw_mant_procvaccomenca"
end type

event dw_2::itemchanged;Long		ll_null
Integer	li_codigo
String	ls_columna
Date ld_fecha

SetNull(ll_null)
ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		IF NOT iuo_cliente.Existe(Integer(data), TRUE, sqlca) THEN
			dw_2.Object.clie_codigo[1]	=	ll_Null
			RETURN 1
		ELSE
			istr_mant.argumento[7]	= data
		END IF
		
	CASE "orpr_numero"	
		
		IF iuo_doctointerno.Existe(Integer(istr_mant.argumento[7]),	&
											gstr_ParamPlanta.CodigoPlanta,	&
											Integer(istr_Mant.Argumento[2]), &
											Long(data),TRUE,Sqlca) THEN	
											
			istr_mant.argumento[3]	= data
			
			IF IsNull(istr_Mant.Argumento[4] ) THEN 	istr_Mant.Argumento[4] = String(iuo_doctointerno.ld_Fecha)
					
			IF ExisteEncabezado(Long(Data)) THEN
				Parent.TriggerEvent("ue_recuperadatos")
			ELSE
				dw_2.object.orpr_fecpro[1] = iuo_doctointerno.ld_fecha
				dw_2.object.espe_codigo[1] = iuo_doctointerno.especie
				dw_2.Object.orpr_estado[1]	= iuo_doctointerno.estado			
     		END IF	
		ELSE
			dw_2.Object.orpr_numero[1]	=	ll_Null
			RETURN 1
		END IF
		
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		dw_2.Object.orpr_numero[1]	=	ll_Null
		
	CASE "opve_fecvac"	
		ld_fecha = date(data)
		IF  ld_fecha > date(idt_fechasistema) THEN
			MessageBox("Atención","La fecha del vaciado no puede ser mayor a la fecha actual.")
			dw_2.Object.orpr_fecpro[1]	=	date(idt_fechasistema)
			Return 1
		ELSE
			istr_Mant.Argumento[4] = DATA
			IF existeencabezado(dw_2.Object.orpr_numero[1]) THEN
				parent.TriggerEvent("ue_recuperadatos")
			END IF 
		END IF	
	
	CASE "opve_turno"
		istr_mant.argumento[5] = Data
		IF istr_mant.argumento[3] <> "" THEN
			IF iuo_doctointerno.Existe(Integer(istr_mant.argumento[7]),	&
												gstr_ParamPlanta.CodigoPlanta,	&
												Integer(istr_Mant.Argumento[2]), &
												Long(istr_Mant.Argumento[3]),True,Sqlca) THEN	
										
				IF existeencabezado(long(istr_Mant.Argumento[3])) THEN
					parent.TriggerEvent("ue_recuperadatos")
				ELSE
					dw_2.object.orpr_fecpro[1] = iuo_doctointerno.ld_fecha
					dw_2.object.espe_codigo[1] = iuo_doctointerno.especie
					dw_2.Object.orpr_estado[1]	= iuo_doctointerno.estado	
				END IF
			END IF		  
		END IF
		
END CHOOSE

Habilitadetalle(ls_columna)

end event

event dw_2::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "b_orden"
		
		buscaorden()
		
END CHOOSE		
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_procvaciadocomercial
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_procvaciadocomercial
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_procvaciadocomercial
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_procvaciadocomercial
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_procvaciadocomercial
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_procvaciadocomercial
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_procvaciadocomercial
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_procvaciadocomercial
end type

type dw_3 from datawindow within w_maed_spro_procvaciadocomercial
boolean visible = false
integer x = 2967
integer y = 140
integer width = 283
integer height = 188
integer taborder = 70
boolean bringtotop = true
string title = "Todos los Vaciados para la Orden"
string dataobject = "dw_mues_lotescomer_vaciados"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

