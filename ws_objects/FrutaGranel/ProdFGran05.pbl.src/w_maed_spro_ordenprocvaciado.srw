$PBExportHeader$w_maed_spro_ordenprocvaciado.srw
forward
global type w_maed_spro_ordenprocvaciado from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_spro_ordenprocvaciado
end type
type dw_4 from datawindow within w_maed_spro_ordenprocvaciado
end type
type dw_10 from datawindow within w_maed_spro_ordenprocvaciado
end type
end forward

global type w_maed_spro_ordenprocvaciado from w_mant_encab_deta_csd
integer width = 5344
integer height = 2140
string title = "Confirmacion de Vaciado"
string menuname = ""
boolean minbox = false
dw_3 dw_3
dw_4 dw_4
dw_10 dw_10
end type
global w_maed_spro_ordenprocvaciado w_maed_spro_ordenprocvaciado

type variables
uo_lineapacking 								iuo_LineaPacking	
uo_spro_ordenproceso						iuo_ordenproceso	
uo_plantadesp									iuo_plantalote
uo_control_historico_proceso				iuo_historico

datawindowchild     							idwc_planta, idwc_linea, idwc_tipoenva, idwc_envase, idwc_calidad

str_envase										istr_Envase

Datetime 				 						idt_fechasistema

w_mant_deta_spro_vaciadoproceso_bins	iw_detalles
str_mant											istr_mant2
end variables

forward prototypes
public subroutine buscacalidad (string as_calidad)
public subroutine buscadetallelotes ()
public subroutine habilitaencab (boolean habilita)
public function long buscabultos (string as_lote, integer ai_tipoenva, integer ai_enva)
public function long buscalotesrepetidos (string as_lote, integer is_tipoen, integer is_envase, long al_fila)
public function boolean duplicado (string as_columna, string as_valor)
public function boolean habilitabultos (string as_columna, string as_valor)
public subroutine buscaorden ()
public function long buscabultosyaingresados (string as_lote, integer ai_tipoen, integer ai_envase)
public subroutine habilitadetalle (string as_columna)
public subroutine actualizatotalbultos (string as_lote, integer is_tipoen, integer is_envase, long al_fila, long al_bultos)
public function boolean existeencabezado (long al_numero)
public function boolean noexistecliente (integer al_codigo)
public subroutine llenanombres (long al_productor, integer ai_especie, integer ai_variedad, integer ai_cliente)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine eliminarbins (integer ai_row)
public subroutine pesooriginalbins ()
public subroutine pesooriginalbultos ()
public subroutine pesosbultoabulto ()
public function boolean historial ()
public function boolean tienetarjas (long al_numero)
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

public subroutine buscadetallelotes ();Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=  istr_mant.argumento[2]
lstr_busq.argum[3]	=  istr_mant.argumento[3]
lstr_busq.argum[4]	=  istr_mant.argumento[16]

OpenWithParm(w_busc_detalle_lotesvaciados, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("lote_codigo")
	dw_1.SetFocus()
ELSE
	dw_1.Object.lote[il_fila] = 	lstr_busq.argum[4]
	
	dw_1.Object.lote_pltcod[il_fila] = 	Integer(lstr_busq.argum[9])
	dw_1.Object.lote_espcod[il_fila] = 	Integer(lstr_busq.argum[10])
	dw_1.Object.lote_codigo[il_fila] = 	Integer(lstr_busq.argum[11])	

	dw_1.Object.enva_tipoen[il_fila]	=	Integer(lstr_busq.argum[5])
	
	dw_1.GetChild("enva_codigo", idwc_envase)
	idwc_envase.SetTransObject(SqlCa)
	idwc_envase.Retrieve(integer(lstr_busq.argum[5]))
	
	dw_1.Object.enva_codigo[il_fila]	=	integer(lstr_busq.argum[6])
	dw_1.Object.enva_nombre[il_fila]	=	lstr_busq.argum[7]
	dw_1.Object.opvd_canbul[il_fila]	=	long(lstr_busq.argum[8])
	dw_1.Object.bultostot[il_fila]	=	long(lstr_busq.argum[8])
	
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

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.opve_fecvac.Protect				=	0
	dw_2.Object.opve_turno.Protect				=	0
	dw_2.Object.orpr_tipord.Protect				=	0
	dw_2.Object.orpr_numero.Protect				=	0
	dw_2.Object.line_codigo.Protect				=	0

	dw_2.Object.clie_codigo.Color		=	0
	dw_2.Object.opve_fecvac.Color	=	0
	dw_2.Object.opve_turno.Color		= 	0	
	dw_2.Object.orpr_tipord.Color		=	0
	dw_2.Object.orpr_numero.Color	=	0
	dw_2.Object.line_codigo.Color		=	0	
	
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.opve_fecvac.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.opve_turno.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_tipord.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.line_codigo.BackGround.Color		=	RGB(255,255,255)
	
	dw_2.Object.b_orden.visible					=  1
ELSE
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.opve_fecvac.Protect				=	1
	dw_2.Object.opve_turno.Protect				=	1
	dw_2.Object.orpr_tipord.Protect				=	1
	dw_2.Object.orpr_numero.Protect				=	1
	dw_2.Object.line_codigo.Protect				=	1

	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.opve_fecvac.Color	=	RGB(255,255,255)
	dw_2.Object.opve_turno.Color		=	RGB(255,255,255)
	//dw_2.Object.orpr_tipord.Color		=	RGB(255,255,255)
	dw_2.Object.orpr_numero.Color	=	RGB(255,255,255)
	dw_2.Object.line_codigo.Color		=	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.opve_fecvac.BackGround.Color		=	553648127
	dw_2.Object.opve_turno.BackGround.Color		=	553648127
	dw_2.Object.orpr_tipord.BackGround.Color		=	553648127
	dw_2.Object.orpr_numero.BackGround.Color	=	553648127
	dw_2.Object.line_codigo.BackGround.Color		=	553648127
	
	dw_2.Object.b_orden.visible					=  0
END IF
end subroutine

public function long buscabultos (string as_lote, integer ai_tipoenva, integer ai_enva);String   ls_Nombre, ls_envase, ls_codenva
Integer  li_tipodoc, li_orden, li_lotepl, li_lotesp, li_lote, li_tipomov, li_Cliente
Long     ll_Bultos, ll_numero

li_tipodoc	=	Integer(istr_mant.argumento[2])
li_orden		=	Integer(istr_mant.argumento[3])
li_Cliente	=	Integer(istr_mant.argumento[16])

li_lotepl   =	Integer(Mid(as_lote,1,4))
li_lotesp	=	Integer(mid(as_lote,5,2))
li_lote		=	Integer(mid(as_lote,7,10))

SELECT	distinct tpmv_codigo, mfge_numero
	INTO	:li_tipomov, :ll_numero
	FROM	dbo.spro_movtofrutagranenca
  WHERE	plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
    AND 	tpmv_codigo =  21
    AND  defg_tipdoc =  :li_Tipodoc
	 AND  defg_docrel =  :li_orden
	 AND  clie_codigo =  :li_Cliente; 
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Encabezado")

ELSEIF sqlca.SQLCode<>100 THEN
	
	 SELECT	mfgd_bulent
		INTO	:ll_Bultos
		FROM	dbo.spro_movtofrutagrandeta
  	  WHERE	plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
   	 AND  tpmv_codigo =  :li_tipomov
		 AND  mfge_numero =  :ll_numero
		 AND  lote_pltcod =  :li_lotepl
		 AND  lote_espcod	=	:li_lotesp
		 AND  lote_codigo	=	:li_lote
		 AND  enva_tipoen =  :ai_tipoenva
		 AND  enva_codigo =  :ai_enva
		 AND  clie_codigo =  :li_Cliente;
   
	IF sqlca.SQLCode = -1 THEN
	   F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Detalle")
   ELSEIF sqlca.SQLCode<>100 THEN
		
   END IF

END IF

Return ll_Bultos
end function

public function long buscalotesrepetidos (string as_lote, integer is_tipoen, integer is_envase, long al_fila);Long ll_bultos, ll_fila
Integer li_tipoen, li_envase
String ls_lotefila

ll_bultos=0

FOR ll_fila=1 TO dw_1.RowCount()
   IF ll_fila<>al_fila THEN
		ls_lotefila=dw_1.Object.lote[ll_fila]
		li_tipoen  =dw_1.Object.enva_tipoen[ll_fila]
		li_envase  =dw_1.Object.enva_codigo[ll_fila]
		
		IF as_lote=ls_lotefila AND is_tipoen=li_tipoen AND is_envase=li_envase THEN
			ll_bultos = ll_bultos + dw_1.Object.opvd_canbul[ll_fila]
		END IF
		
	END IF
NEXT	
RETURN ll_bultos
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
string   ls_loteco1, ls_tipoen1, ls_envase1, ls_planta1, ls_hora1
string   ls_loteco2, ls_tipoen2, ls_envase2, ls_planta2, ls_hora2

ls_loteco1 = string(dw_1.Object.lote_codigo[il_fila])
ls_planta1 = string(dw_1.Object.lote_pltcod[il_fila])
ls_tipoen1 = string(dw_1.Object.enva_tipoen[il_fila])
ls_envase1 = string(dw_1.Object.enva_codigo[il_fila])
ls_hora1   = string(dw_1.Object.opvd_horava[il_fila],"hh:mm")

CHOOSE CASE as_columna
	CASE "lote_codigo"
		ls_loteco1 = as_valor
	
	CASE "lote_pltcod"
		ls_planta1 = as_valor
		
	CASE "enva_tipoen"
		ls_tipoen1 = as_valor	
	
	CASE "enva_codigo"
		ls_envase1 = as_valor
		
	CASE "opvd_harava"
		ls_hora1 = MID(as_valor,12,5)	
		
END CHOOSE		

FOR ll_fila=1 To dw_1.Rowcount()
	IF ll_fila<>il_fila THEN
      ls_loteco2 = string(dw_1.Object.lote_codigo[ll_fila])
		ls_planta2 = string(dw_1.Object.lote_pltcod[ll_fila])
		ls_tipoen2 = string(dw_1.Object.enva_tipoen[ll_fila])
		ls_envase2 = string(dw_1.Object.enva_codigo[ll_fila])
		ls_hora2   = string(dw_1.Object.opvd_horava[ll_fila],"hh:mm")
	
		IF ls_loteco1 = ls_loteco2 AND ls_planta1 = ls_planta2 AND ls_tipoen1 = ls_tipoen2 AND &
		   ls_envase1 = ls_envase2 AND ls_hora1   = ls_hora2 THEN
			MessageBox("Error", "La hora de vaciado para el lote ya fue ingresada anteriormente", Information!, Ok!)
			RETURN True
		END IF	
	END IF
NEXT	

RETURN False

end function

public function boolean habilitabultos (string as_columna, string as_valor);Boolean	lb_Estado = True
Long     ll_Bultos, ll_bultosrep, ll_bultosyaingresados
String   ls_lote, ls_null
Integer  li_tipo, li_enva

SetNull(ls_Null)
 
IF as_Columna <> "lote" AND &
	((dw_1.Object.lote[il_fila] = "") OR isnull(dw_1.Object.lote[il_fila])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "enva_tipoen" AND &
	((dw_1.Object.enva_tipoen[il_fila] = 0) OR (IsNull(dw_1.Object.enva_tipoen[il_fila]))) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "enva_codigo" AND &
	(dw_1.Object.enva_codigo[il_fila] = 0 OR IsNull(dw_1.Object.enva_codigo[il_fila])) THEN
	lb_Estado = False
END IF

IF lb_estado THEN
	ls_lote	=	dw_1.Object.lote[il_fila]
	li_tipo	=	dw_1.Object.enva_tipoen[il_fila]
	li_enva	=	dw_1.Object.enva_codigo[il_fila]
	
	IF as_columna="lote" 		 THEN ls_lote	=	as_valor
	IF as_columna="enva_tipoen" THEN li_tipo	=	integer(as_valor)
	IF as_columna="enva_codigo" THEN li_enva	=	integer(as_valor)
	
	IF ls_lote<>"" AND isnull(li_tipo)=FALSE AND isnull(li_enva)=FALSE THEN
		
		ll_Bultos=BuscaBultos(ls_lote,li_tipo, li_enva)
		
		IF ll_Bultos<= 0 THEN
			MessageBox("Atención","Número de Lote Ingresado No posee Bultos de Movimiento. Debe Ingresar Otro.")
			dw_1.Object.lote[il_fila]				=	ls_null
			dw_1.Object.lote_codigo[il_fila]    =  integer(ls_null)
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
				ll_bultosrep = buscalotesrepetidos(ls_lote,li_tipo,li_enva,il_fila)
			ELSE	 
				ll_bultosrep = 0
			END IF	 
			
		   ll_bultosyaingresados = buscabultosyaingresados(ls_lote,li_tipo,li_enva)
			
		   IF isnull(ll_bultosyaingresados) THEN ll_bultosyaingresados = 0
			
			ll_bultos = ll_bultos - ll_bultosyaingresados - ll_bultosrep
			IF ll_bultos<=0 THEN
				MessageBox("Atención","El lote ya fue ingresado en su totalidad. Debe Ingresar Otro.")
				dw_1.Object.lote[il_fila]				=	ls_null
				dw_1.Object.lote_codigo[il_fila]    =  integer(ls_null)
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
				dw_1.Object.opvd_canbul[il_fila] 	=	ll_Bultos
				dw_1.Object.bultostot[il_fila]   	=	0      //ll_bultos 
				actualizatotalbultos(ls_lote,li_tipo, li_enva,il_fila,0)
			END IF
      END IF			
	END IF	
END IF	

RETURN TRUE
end function

public subroutine buscaorden ();Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=	"0"
lstr_busq.argum[3]	=  istr_mant.argumento[2]
lstr_busq.argum[4]	=  istr_mant.argumento[16]

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1] <> "0" THEN
	
	IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Integer(lstr_busq.argum[6]),True,Sqlca, &
											Integer(istr_mant.argumento[16])) THEN	
		IF Not manbin_especie(gstr_ParamPlanta.CodigoPlanta, iuo_ordenproceso.especie, True, sqlca) THEN
			dw_2.SetItem(1,"orpr_numero", Integer(ls_Nula))
			RETURN
			
		END IF
		
		IF Not TieneTarjas(Integer(lstr_busq.argum[6])) THEN
			dw_2.SetItem(1,"orpr_numero", Integer(ls_Nula))
			RETURN
			
		END IF
		
		istr_mant.argumento[3]	= lstr_busq.argum[6]
		dw_2.SetItem(1,"orpr_numero",long(lstr_busq.argum[6]))
		
		IF existeencabezado(Long(istr_mant.argumento[3])) THEN
			TriggerEvent("ue_recuperadatos")
			
		ELSE
			dw_2.Object.line_codigo[1] = iuo_ordenproceso.linea
			dw_2.object.orpr_fecpro[1] = iuo_ordenproceso.fechaorden
			dw_2.object.prod_codigo[1] = iuo_ordenproceso.productor
			dw_2.object.espe_codigo[1] = iuo_ordenproceso.especie
			dw_2.object.vari_codigo[1] = iuo_ordenproceso.variedad
			dw_2.Object.orpr_estado[1] = iuo_ordenproceso.estado

			IF date(string(iuo_ordenproceso.fechaorden,"dd/mm/yyyy")) > &
				date(string(dw_2.Object.opve_fecvac[1],"dd/mm/yyyy")) THEN
				dw_2.Object.opve_fecvac[1] = iuo_ordenproceso.fechaorden
			END IF	

			LlenaNombres(iuo_ordenproceso.productor,iuo_ordenproceso.especie,&
							 iuo_ordenproceso.variedad,Integer(istr_mant.argumento[16]))
			Habilitadetalle("orpr_numero")
		END IF
	ELSE
		dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
		
	END IF
END IF
end subroutine

public function long buscabultosyaingresados (string as_lote, integer ai_tipoen, integer ai_envase);Long 		ll_fila, ll_bultos=0
Integer 	li_lotepl, li_lotesp, li_lote, li_turno, li_turnofila, li_Cliente
Date 		ldt_fechava, ldt_fechafila

li_lotepl   =	Integer(Mid(as_lote,1,4))
li_lotesp	=	Integer(mid(as_lote,5,2))
li_lote		=	Integer(mid(as_lote,7,10))
ldt_fechava =  dw_2.Object.opve_fecvac[1]
li_turno		=  dw_2.Object.opve_turno[1]
li_Cliente	=	Integer(istr_mant.argumento[16])

dw_3.SetFilter("lote_pltcod = " + string(li_lotepl) + " AND " + &
               "lote_espcod = " + string(li_lotesp) + " AND " + &
               "lote_codigo = " + string(li_lote)   + " AND " + &
               "clie_codigo = " + string(li_Cliente) + " AND " + &
					"enva_tipoen = " + string(ai_tipoen) + " AND " + &
					"enva_codigo = " + string(ai_envase) )
dw_3.Filter()					

FOR ll_fila = 1 TO dw_3.RowCount()
	li_turnofila 	=	dw_3.Object.opve_turno[ll_fila]
	ldt_fechafila 	=	dw_3.Object.opve_fecvac[ll_fila]
	IF li_turnofila <> li_turno OR ldt_fechafila  <> ldt_fechava THEN
		
		ll_bultos = ll_bultos + dw_3.Object.opvd_canbul[ll_fila]
		
	END IF	 
NEXT	

IF isnull(ll_bultos) THEN ll_Bultos = 0

dw_3.SetFilter("")
dw_3.Filter()

RETURN ll_bultos

end function

public subroutine habilitadetalle (string as_columna);Boolean	lb_Estado = True
Date     ldt_fecha, ld_fechavac
Integer  li_planta, li_tipord, li_turnovac, li_Cliente
Long     ll_orden

li_Cliente	=	Integer(istr_mant.argumento[16])

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

IF as_Columna <> "opve_estado" AND &
	(dw_2.Object.opve_estado[1] = "C" OR IsNull(dw_2.Object.opve_estado[1])) THEN
	lb_Estado = False
END IF

IF lb_estado THEN
  pb_ins_det.Enabled = lb_estado
  dw_2.accepttext()
  li_planta 	=	dw_2.Object.plde_codigo[1]
  li_tipord 	=	dw_2.Object.orpr_tipord[1]
  ll_orden		=	dw_2.Object.orpr_numero[1]
  ld_fechavac 	=	dw_2.Object.opve_fecvac[1]
  li_turnovac  =  dw_2.Object.opve_turno[1]  
  dw_3.SetTransObject(SQLCA)
  dw_3.Retrieve(li_planta,li_tipord, ll_orden,li_Cliente)
  
END IF	

end subroutine

public subroutine actualizatotalbultos (string as_lote, integer is_tipoen, integer is_envase, long al_fila, long al_bultos);Long    ll_fila
Integer li_tipoen, li_envase
String  ls_lotefila

FOR ll_fila=1 TO dw_1.RowCount()
   IF ll_fila<>al_fila THEN
		ls_lotefila=dw_1.Object.lote[ll_fila]
		li_tipoen  =dw_1.Object.enva_tipoen[ll_fila]
		li_envase  =dw_1.Object.enva_codigo[ll_fila]
		
		IF as_lote=ls_lotefila AND is_tipoen=li_tipoen AND is_envase=li_envase THEN
			
			dw_1.Object.bultostot[ll_fila] = al_bultos
		END IF
		
	END IF
NEXT	

end subroutine

public function boolean existeencabezado (long al_numero);Boolean	lb_Retorno
Integer	li_Planta, li_tpmv, li_Cantidad, li_turno, li_Cliente
Date     ldt_fecha
String	ls_fecha

ldt_Fecha		=	Date(left(istr_Mant.Argumento[4], 10))
li_tpmv			=	Integer(istr_Mant.Argumento[2])
li_turno       =  Integer(istr_Mant.Argumento[5])
ls_fecha			=	String(ldt_fecha,'yyyymmdd')
li_Cliente		=	Integer(istr_mant.argumento[16])

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dbo.spro_ordenprocvacenca as sop
	WHERE	sop.plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
	AND   sop.orpr_tipord	=	:li_tpmv
	AND	sop.orpr_numero	=	:al_numero
	AND 	Convert(char(10),sop.opve_fecvac,112)	=	:ls_fecha
	AND 	sop.opve_turno		=	:li_turno
	AND   sop.clie_codigo   =  :li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvaenca")
	
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 and li_cantidad>0 THEN

	lb_Retorno	=	True

END IF

RETURN lb_Retorno
end function

public function boolean noexistecliente (integer al_codigo);Integer	li_cliente

SELECT clie_codigo
INTO	:li_cliente
FROM	dbo.clientesprod
WHERE	clie_codigo	=	:al_codigo;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ClientesProd" )
		Return True			
ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
		Return True						
END IF

Return False

end function

public subroutine llenanombres (long al_productor, integer ai_especie, integer ai_variedad, integer ai_cliente);String   ls_Nombre, ls_envase, ls_codenva
Integer  li_tipoenva, li_codenva, ls_enavse

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
  WHERE	prod_codigo	=	:al_productor;
//  AND    clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Productor")

ELSEIF sqlca.SQLCode<>100 THEN
	dw_2.object.prod_nombre[1]=ls_nombre
END IF

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dbo.especies
  WHERE	espe_codigo	=	:ai_especie;
//  AND    clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Especie")

ELSEIF sqlca.SQLCode<>100 THEN
	dw_2.object.espe_nombre[1]=ls_nombre
END IF

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dbo.variedades
  WHERE	espe_codigo	=	:ai_especie
    AND  vari_codigo =  :ai_variedad;
//    AND  clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Variedades")

ELSEIF sqlca.SQLCode<>100 THEN
	dw_2.object.vari_nombre[1]=ls_nombre
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF Historial() THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF Historial() THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine eliminarbins (integer ai_row);Integer li_i

FOR li_i 	=	1 TO dw_4.RowCount()
	
	IF dw_4.Object.opva_secuen[li_i] = ai_row THEN
		dw_4.DeleteRow(li_i)
		li_i = li_i - 1
	ELSEIF dw_4.Object.opva_secuen[li_i] > ai_row THEN
		dw_4.Object.opva_secuen[li_i] = dw_4.Object.opva_secuen[li_i] - 1
	END IF
	
NEXT
end subroutine

public subroutine pesooriginalbins ();//Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote, li_tarja
//Integer	li_TipEn, li_CodEn, li_bultos, li_Cliente, li_Planta, li_Numero, li_especie, li_tipoenva, li_envase
//String	ls_Calid, ls_calidad
//Decimal	ldec_KilOri, ldec_Tara,ld_tarabasepallet
//	
//ll_fila 			=	dw_1.GetRow() 
//
//IF ll_fila < 1 THEN RETURN
//
//ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
//ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
//ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
//li_bultos	=	dw_1.Object.opvd_canbul[ll_fila]
//li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
//li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
//ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
//ll_lote		=	dw_1.Object.lote_codigo[ll_fila]
//
//IF istr_mant2.Argumento[3] <> '9' THEN
//
//	SELECT Sum(mfgp.mfgp_pesore)
//	  INTO :ldec_KilOri
//	  FROM dbo.spro_movtofrutagranpesa as mfgp
//	 WHERE mfgp.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
//		AND lote_codigo = :ll_lote;
//	
//	IF sqlca.SQLCode <> 0 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
//	ELSE
//		
//		IF gstr_paramplanta.palletdebins THEN
//			li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
//			li_Planta 	= 	Integer(istr_Mant.Argumento[2])
//			li_Numero	=	Integer(istr_mant.Argumento[4])
//			ll_lote		=	dw_1.Object.lote_codigo[il_fila]
//			li_especie	=	dw_1.Object.lote_espcod[il_fila]
//			li_tarja		=	dw_1.Object.fgmb_tibapa[il_fila]
//			
//			SELECT enva_tipoen, enva_codigo, cale_calida
//			  INTO :li_TipoEnva, :li_Envase, :ls_Calidad
//			  FROM dbo.spro_bins
//			 WHERE( clie_codigo	=	:li_cliente	)
//				AND( plde_codigo	=	:li_Planta	)
//				AND( bins_numero	=	:li_tarja	);
//					
//			IF sqlca.SQLCode < 0 THEN
//				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Envases")
//			ELSEIF sqlca.SQLCode = 100 THEN
//				MessageBox("Error", "No se encuentra el Movimiento de Envase Correspondiente a este Bulto")
//				RETURN
//			END IF
//			
//			SELECT cale_pesoen
//			  INTO :ld_tarabasepallet
//			  FROM dbo.spro_calicosechero
//			 WHERE enva_tipoen =	 :li_TipoEnva
//				AND enva_codigo =  :li_Envase
//				AND cale_calida =  :ls_Calidad;
//			IF sqlca.SQLCode <> 0 THEN
//				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
//				RETURN
//			END IF
//		END IF
//		
//		SELECT cale_pesoen
//		  INTO :ldec_Tara
//		  FROM dbo.spro_calicosechero
//		 WHERE enva_tipoen = :li_TipEn
//			AND enva_codigo = :li_CodEn
//			AND cale_calida = :ls_Calid;
//		 
//		IF sqlca.SQLCode <> 0 THEN
//			F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
//		ELSE
//			dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
//		END IF	
//	END IF
//ELSE
//	SELECT	Sum(lfgd.lfcd_kilnet)
//		INTO	:ldec_KilOri
//		FROM	dbo.spro_lotesfrutacomdeta as lfgd
//		WHERE 	lfgd.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
//			 AND	lfgd.lofc_lotefc = :ll_lote;
//			 
//	IF sqlca.SQLCode <> 0 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Comerciales")
//		dw_1.object.opvd_kilori[ll_fila]	=	0
//	ELSE
//		dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri, 2)
//	END IF	
//
//END IF
end subroutine

public subroutine pesooriginalbultos ();//Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote, li_tarja
//Integer	li_TipEn, li_CodEn, li_bultos, li_Cliente, li_Planta, li_Numero, li_especie, li_tipoenva, li_envase
//String	ls_Calid, ls_calidad
//Decimal	ldec_KilOri, ldec_Tara,ld_tarabasepallet
//	
//ll_fila 			=	dw_1.GetRow() 
//
//IF ll_fila < 1 THEN RETURN
//
//ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
//ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
//ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
//li_bultos	=	dw_1.Object.opvd_canbul[ll_fila]
//li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
//li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
//ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
//ll_lote		=	dw_1.Object.lote_codigo[ll_fila]
//
//IF istr_mant2.Argumento[3] <> '9' THEN
//
//	SELECT Sum(mfgp.mfgp_pesore)
//	  INTO :ldec_KilOri
//	  FROM dbo.spro_movtofrutagranpesa as mfgp
//	 WHERE mfgp.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
//		AND lote_codigo = :ll_lote;
//	
//	IF sqlca.SQLCode <> 0 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
//	ELSE
//		
//		IF gstr_paramplanta.palletdebins THEN
//			li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
//			li_Planta 	= 	Integer(istr_Mant.Argumento[2])
//			li_Numero	=	Integer(istr_mant.Argumento[4])
//			ll_lote		=	dw_1.Object.lote_codigo[il_fila]
//			li_especie	=	dw_1.Object.lote_espcod[il_fila]
//			li_tarja		=	dw_1.Object.fgmb_tibapa[il_fila]
//			
//			SELECT enva_tipoen, enva_codigo, cale_calida
//			  INTO :li_TipoEnva, :li_Envase, :ls_Calidad
//			  FROM dbo.spro_bins
//			 WHERE( clie_codigo	=	:li_cliente	)
//				AND( plde_codigo	=	:li_Planta	)
//				AND( bins_numero	=	:li_tarja	);
//					
//			IF sqlca.SQLCode < 0 THEN
//				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Envases")
//			ELSEIF sqlca.SQLCode = 100 THEN
//				MessageBox("Error", "No se encuentra el Movimiento de Envase Correspondiente a este Bulto")
//				RETURN
//			END IF
//			
//			SELECT cale_pesoen
//			  INTO :ld_tarabasepallet
//			  FROM dbo.spro_calicosechero
//			 WHERE enva_tipoen =	 :li_TipoEnva
//				AND enva_codigo =  :li_Envase
//				AND cale_calida =  :ls_Calidad;
//			IF sqlca.SQLCode <> 0 THEN
//				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
//				RETURN
//			END IF
//		END IF
//		
//		SELECT cale_pesoen
//		  INTO :ldec_Tara
//		  FROM dbo.spro_calicosechero
//		 WHERE enva_tipoen = :li_TipEn
//			AND enva_codigo = :li_CodEn
//			AND cale_calida = :ls_Calid;
//		 
//		IF sqlca.SQLCode <> 0 THEN
//			F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
//		ELSE
//			dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
//		END IF	
//	END IF
//ELSE
//	SELECT	Sum(lfgd.lfcd_kilnet)
//		INTO	:ldec_KilOri
//		FROM	dbo.spro_lotesfrutacomdeta as lfgd
//		WHERE 	lfgd.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
//			 AND	lfgd.lofc_lotefc = :ll_lote;
//			 
//	IF sqlca.SQLCode <> 0 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Comerciales")
//		dw_1.object.opvd_kilori[ll_fila]	=	0
//	ELSE
//		dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri, 2)
//	END IF	
//
//END IF
end subroutine

public subroutine pesosbultoabulto ();Integer	li_filas, li_planta, li_especie, li_TipEnv, li_CodEnv
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
	
	SELECT lote_kilpro INTO :ldec_kilos_prom_originales
		FROM dbo.spro_lotesfrutagranel
		WHERE lote_pltcod	=	:li_planta
		  AND lote_espcod	=	:li_especie
		  AND lote_codigo	=	:ll_lote;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
	ELSE
		SELECT cale_pesoen INTO :ldec_kilos_tara
			FROM dbo.spro_calicosechero
			WHERE enva_tipoen	=	:li_TipEnv
				AND enva_codigo	=	:li_CodEnv
				AND cale_calida	=	:ls_calidad;
				
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
		ELSE
			//peso neto vaciado
			dw_1.Object.opvd_pesone[li_filas] 	=	ldec_kilos_prom_originales * ll_bultos 
			//peso bruto vaciado
			dw_1.Object.opvd_pesobr[li_filas] 	= 	( ldec_kilos_prom_originales * ll_bultos ) + ( ldec_kilos_tara * ll_bultos )
			//peso promedio neto
			dw_1.Object.opvd_kilpro[li_filas]	=	ldec_kilos_prom_originales
			//peso neto original
			dw_1.Object.opvd_kilori[li_filas]	=	ldec_kilos_prom_originales * ll_bultos 
		END IF
		
	END IF
	
NEXT
end subroutine

public function boolean historial ();Boolean			lb_retorno	=	True
Integer			ll_fila
DwItemStatus	li_dwitemstatus
Integer			li_cliente, li_tipord, li_codmod, li_tipmov
Long				ll_planta, ll_proceso
Date 				ld_fecmov
Time				lt_hormov
String 			ls_pcname

ll_fila 				= 	dw_2.RowCount()

IF ll_fila > 0 THEN
	li_dwitemstatus	=	dw_2.GetItemStatus(ll_Fila, 0, Primary!)
	
	IF li_dwitemstatus <> New! AND li_dwitemstatus <> NotModified! OR dw_2.DeletedCount() > 0 THEN
		li_cliente		=	Integer(istr_mant.argumento[16])
		ll_planta		=	dw_2.Object.plde_codigo[ll_fila]
		li_tipord		=	dw_2.Object.orpr_tipord[ll_fila]
		ll_proceso		=	dw_2.Object.orpr_numero[ll_fila]
		ld_fecmov		=	Date(f_fechahora())
		lt_hormov		=	Time(f_fechahora())
		ls_pcname		=	gstr_us.computador
				
		li_tipmov		=	3//3 vaciado
		
		CHOOSE CASE li_dwitemstatus
			CASE NewModified!
				li_codmod	=	1
				
			CASE DataModified!
				li_codmod	=	2
				
		END CHOOSE
	
		IF dw_2.DeletedCount() > 0 THEN li_codmod	=	2
			
		lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																	ll_proceso, li_codmod, li_tipmov, &
																	ld_fecmov,  lt_hormov, ls_pcname, &
																	True, Sqlca)
	END IF
END IF

IF NOT lb_retorno THEN Return lb_retorno

dw_2.RowsCopy(1, dw_2.DeletedCount(), Delete!, dw_10, 1, Primary!)

IF dw_10.RowCount() > 0 THEN		
	li_cliente		=	Integer(istr_mant.argumento[16])
	ll_planta		=	dw_10.Object.plde_codigo[1]
	li_tipord		=	dw_10.Object.orpr_tipord[1]
	ll_proceso		=	dw_10.Object.orpr_numero[1]
	ld_fecmov		=	Date(f_fechahora())
	lt_hormov		=	Time(f_fechahora())
	ls_pcname		=	gstr_us.computador
			
	li_tipmov		=	3//3 vaciado
	li_codmod		=	3//1 creacion, 2 modificacion, 3 eliminacion
	
	lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																ll_proceso, li_codmod, li_tipmov, &
																ld_fecmov,  lt_hormov, ls_pcname, &
																True, Sqlca)
	dw_10.Reset()
	
END IF

Return lb_retorno
end function

public function boolean tienetarjas (long al_numero);Boolean	lb_Retorno = True
Integer	li_Planta, li_tpmv, li_Cantidad, li_turno, li_Cliente

li_tpmv			=	Integer(istr_Mant.Argumento[2])
li_turno       =  Integer(istr_Mant.Argumento[5])
li_Cliente		=	Integer(istr_mant.argumento[16])

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dbo.spro_ordenprocdeta_cajasprod as sop
	WHERE	sop.plde_codigo	=	:gstr_ParamPlanta.CodigoPlanta
	AND   sop.orpr_tipord	=	:li_tpmv
	AND	sop.orpr_numero	=	:al_numero
	AND   sop.clie_codigo   =  :li_Cliente
	AND 	sop.capr_estado	=	1;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocdeta_cajasprod")
	
	RETURN FALSE
ELSEIF li_cantidad < 1 THEN
	MessageBox("Validación de Adhesivos", "No se encuentran adhesivos aprobados para "	+ 	& 
													  "la orden que se intenta vaciar.~r~n" 			+	&
													  "Favor de dar aviso a encargado de packing")
	Return False
END IF

RETURN lb_Retorno
end function

on w_maed_spro_ordenprocvaciado.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_10=create dw_10
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_10
end on

on w_maed_spro_ordenprocvaciado.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_10)
end on

event open;/* 
	Argumentos
	
	istr_Mant.Argumento[01]	=	Código Planta
	istr_Mant.Argumento[02]	=	Tipo Orden
	istr_Mant.Argumento[03]	=	Numero
	istr_Mant.Argumento[04]	=	Fecha Vaciado
	istr_Mant.Argumento[05]	=	Turno
	istr_Mant.Argumento[06]	=	Estado Orden de Vaciado
	istr_Mant.Argumento[07]	=	
	istr_Mant.Argumento[16]	=	Cliente
*/

x												= 0
y												= 0

This.Height									= 2520
im_menu										= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

IF gstr_paramplanta.binsabins OR gstr_paramplanta.palletdebins OR  gstr_paramplanta.bultobins THEN
	dw_1.DataObject = "dw_mant_mues_spro_ordenprocvacdeta_pesos"
END IF

istr_Mant.Argumento[1]					=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]					=	"4"
istr_Mant.Argumento[3]					=	""
istr_Mant.Argumento[4]					=	""
istr_Mant.Argumento[5]					=	""
istr_Mant.Argumento[6]					=	"V"
istr_Mant.Argumento[16]				=	String(gi_Codexport)

dw_2.Object.clie_codigo[1] 			=  Integer(istr_mant.argumento[16])

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_2.GetChild("line_codigo", idwc_linea)
idwc_linea.SetTransObject(sqlca)
idwc_linea.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("cale_calida", idwc_calidad)
idwc_calidad.SetTransObject(SqlCa)
idwc_calidad.InsertRow(0)

dw_1.GetChild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(SqlCa)
idwc_envase.Retrieve(-1)


dw_1.GetChild("enva_tipoen", idwc_tipoenva)
idwc_tipoenva.SetTransObject(SqlCa)
idwc_tipoenva.Retrieve()
idwc_tipoenva.SetFilter("tien_usoenv = 1")
idwc_tipoenva.Filter()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_LineaPacking	=	Create uo_lineapacking
iuo_ordenproceso	=	Create uo_spro_ordenproceso
iuo_plantalote    	=	Create uo_plantadesp
iuo_historico		=	Create uo_control_historico_proceso

dw_4.SetTransObject(sqlca)
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
			ll_modif1  +=	dw_2.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 ANd ll_modif1 > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","¿Desea Grabar la información?", Question!, YesNoCancel!)
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
dw_2.InsertRow(0)
dw_2.SetFocus()
dw_4.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

idt_fechasistema 			= 	F_fechahora()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.orpr_tipord[1]	=	4
dw_2.Object.opve_fecvac[1]	=	date(idt_fechasistema)
dw_2.Object.opve_estado[1] =  "V"
dw_2.Object.clie_codigo[1] =  Integer(istr_mant.argumento[16])

istr_mant.argumento[2] 		=	"4"
istr_mant.argumento[3]		=	""
istr_mant.argumento[4]		=	String(Date(String(idt_fechasistema,'dd/mm/yyyy')))
istr_mant.argumento[6]		=	"V"

dw_1.GetChild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(SqlCa)
idwc_envase.Retrieve(0)

habilitaencab(TRUE)
dw_2.SetColumn("opve_turno")
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_row, ll_bultos, ll_bultosrep, ll_bultosyaingresados
Date		ld_fecha
Integer  li_Protec=0
String   ls_lote

ld_Fecha	=	Date(left(istr_Mant.Argumento[4], 10))

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Integer(istr_mant.Argumento[3]), &
										  ld_fecha, integer(istr_mant.Argumento[5]), &
										  Integer(istr_mant.Argumento[16]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_2.RowCount() > 0 THEN
			 dw_3.SetTransObject(SQLCA)
		 	 dw_3.Retrieve(dw_2.Object.plde_codigo[1],dw_2.Object.orpr_tipord[1], &
                        dw_2.Object.orpr_numero[1],Integer(istr_mant.Argumento[16]))
		END IF	
		
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Integer(istr_mant.Argumento[3]), &
										  		  ld_fecha, integer(istr_mant.Argumento[5]), &
												  Integer(istr_mant.Argumento[16]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF dw_1.RowCount() > 0 THEN
					
					IF dw_2.Object.orpr_estado[1] = 2 OR dw_2.Object.opve_estado[1] = "C" THEN li_Protec = 1
					
					
					FOR ll_Row=1 To dw_1.RowCount()
						ls_lote	=	String(dw_1.Object.lote_pltcod[ll_row],'0000') + &
					         		String(dw_1.Object.lote_espcod[ll_row],'00') + &
					 					String(dw_1.Object.lote_codigo[ll_row],'0000')
										 
					   dw_1.Object.lote[ll_row]		=	ls_lote
					NEXT 	
					
					FOR ll_row=1 TO dw_1.RowCount()
				    ls_lote	=	String(dw_1.Object.lote_pltcod[ll_row],'0000') + &
					         	String(dw_1.Object.lote_espcod[ll_row],'00') + &
					 				String(dw_1.Object.lote_codigo[ll_row],'0000')
					 					 
					 ll_bultos=BuscaBultos(ls_lote,dw_1.Object.enva_tipoen[ll_row],&
					                       dw_1.Object.enva_codigo[ll_row])
								  
					 IF dw_1.RowCount()>1 THEN			  
					 	ll_bultosrep = buscalotesrepetidos(ls_lote,dw_1.Object.enva_tipoen[ll_row],&
					                                      dw_1.Object.enva_codigo[ll_row], ll_row)
					 ELSE	 
						ll_bultosrep = 0
					 END IF	 
					 			
		   		 ll_bultosyaingresados = buscabultosyaingresados(ls_lote,dw_1.Object.enva_tipoen[ll_row],&
					                                                 dw_1.Object.enva_codigo[ll_row])
			
		   		 IF isnull(ll_bultosyaingresados) THEN ll_bultosyaingresados = 0
			
					 ll_bultos = ll_bultos - ll_bultosyaingresados - ll_bultosrep - dw_1.Object.opvd_canbul[ll_row]
					 
					 dw_1.Object.bultostot[ll_row] = ll_bultos
					 
					 dw_1.Object.protec[ll_row] = li_Protec
					 dw_1.SetItemStatus(ll_row, 0, Primary!, NotModified!)
					 
					NEXT 
					
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
				ELSE
					pb_ins_det.Enabled   =  TRUE
					IF dw_1.RowCount() > 1 THEN
					   pb_eli_det.Enabled	=	TRUE
					ELSE
						pb_eli_det.Enabled	=	FALSE
					END IF	
					pb_grabar.Enabled		=	TRUE
					pb_eliminar.Enabled	=	TRUE
				END IF	
				
				pb_imprimir.Enabled	=	True
			
				HabilitaEncab(False)
				pb_ins_det.SetFocus()
			END IF
				dw_2.SetRedraw(True)
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF
//	DO
//		ll_fila_d	=	dw_4.Retrieve(Integer(istr_mant.Argumento[1]), &
//											Integer(istr_mant.Argumento[16]), &
//											Integer(istr_mant.Argumento[2]), &
//											Integer(istr_mant.Argumento[3]))
//		IF ll_fila_d = -1 THEN
//			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//												Information!, RetryCancel!)
//		END IF											
//	LOOP WHILE respuesta = 1
	
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

dw_1.Enabled	=	True

end event

event ue_nuevo_detalle;
IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
	pb_eli_det.Enabled = True
END IF

IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.bultobins THEN
	il_fila = dw_1.InsertRow(0)
	
	dw_1.Object.lote_pltcod[il_fila] =  Integer(istr_mant.Argumento[1])
	dw_1.Object.lote_espcod[il_fila] =  dw_2.Object.espe_codigo[1]
	
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
	istr_mant2.Argumento[9]		=	String(dw_1.GetRow())
	istr_mant2.Argumento[10]	=	String(dw_4.RowCount())
	istr_mant2.agrega				= 	True
	OpenWithParm(iw_detalles, istr_mant2)
	PesoOriginalBins()
END IF
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.Bultobins THEN
	IF dw_1.RowCount() > 0 THEN
		istr_mant.agrega	= False
		istr_mant.borra	= False
	END IF
ELSE
	istr_mant2.agrega	= False
	Istr_mant2.dw 					= dw_1
	istr_mant2.Argumento[1]	= 	String(dw_2.Object.clie_codigo[1])
	istr_mant2.Argumento[2]	= 	String(dw_2.Object.plde_codigo[1])
	istr_mant2.Argumento[3]	= 	String(dw_2.Object.orpr_tipord[1])
	istr_mant2.Argumento[4]	= 	String(dw_2.Object.orpr_numero[1])
	istr_mant2.Argumento[5]	= 	String(dw_2.Object.espe_codigo[1])
	istr_mant2.Argumento[6]	=	String(dw_1.RowCount())
	istr_mant2.Argumento[7]	=	String(dw_2.Object.opve_fecvac[1],"dd/mm/yyyy")
	istr_mant2.Argumento[8]	=	"1"
	istr_mant2.Argumento[9]	=	String(dw_1.GetRow())
	istr_mant2.Argumento[10]	=	String(dw_4.RowCount())
	OpenWithParm(iw_detalles, istr_mant2)
END IF
end event

event ue_borra_detalle;call super::ue_borra_detalle;Long ll_bultos
String ls_lote
Integer  li_tipen, li_envas

IF istr_Mant.Solo_Consulta THEN RETURN

IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	ll_bultos= dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
	ls_lote  = dw_1.Object.lote[il_fila]
	li_tipen = dw_1.Object.enva_tipoen[il_fila]
	li_envas = dw_1.Object.enva_codigo[il_fila]
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		IF NOT gstr_paramplanta.binsabins THEN eliminarbins(il_fila)
		actualizatotalbultos(ls_lote,li_tipen, li_envas,0,ll_bultos)
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
Integer li_lotepl, li_lotesp, li_lote, li_Contador
Boolean lb_pasada=True
String	ls_Mensaje, ls_Columna[], ls_fecha, ls_hora

Message.DoubleParm = 1

IF dw_1.RowCount()<1 THEN
	MessageBox("Error de Detalle", "No ha Ingresado Detalle.")
	Message.DoubleParm = -1
	RETURN
END IF

FOR ll_fila=1 To dw_1.RowCount()
	
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

	IF dw_1.Object.opvd_canbul[ll_fila] < 0  OR dw_1.Object.opvd_canbul[ll_fila] > 9999 THEN
		MessageBox("Error de Detalle", "En la Fila " + String(ll_fila) + " la cantidad de bultos debe ser mayor a cero y menor a 9.999")
		Message.DoubleParm = -1
		RETURN
	END IF	
NEXT

FOR ll_Fila=1 To dw_1.Rowcount()
	IF dw_1.Object.opvd_canbul[ll_fila]=0 or isnull(dw_1.Object.opvd_canbul[ll_fila]) OR &
		dw_1.Object.lote[ll_fila]="" or isnull(dw_1.Object.lote[ll_fila])Or isnull(dw_1.Object.enva_tipoen[ll_fila]) OR &
		isnull(dw_1.Object.enva_codigo[ll_fila]) THEN
		MessageBox("Error de Detalle", "En la Fila " + String(ll_fila) + " No ha ingresado Datos.")
		Message.DoubleParm = -1
		ll_fila=dw_1.rowcount()
		RETURN
	END IF	
NEXT	

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.Clie_codigo[ll_fila] = 	Integer(istr_mant.argumento[16])
		dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.orpr_tipord[ll_Fila]	=	dw_2.Object.orpr_tipord[1]
		dw_1.Object.orpr_numero[ll_Fila]	=	dw_2.Object.orpr_numero[1]
		dw_1.Object.opve_turno[ll_Fila]	=	dw_2.Object.opve_turno[1]
		dw_1.Object.opve_fecvac[ll_Fila]	=	dw_2.Object.opve_fecvac[1] 
		il_Fila = ll_Fila
		
		IF NOT gstr_paramplanta.binsabins THEN
			IF gstr_paramplanta.palletdebins THEN
				PesoOriginalBultos()
			ELSE
				//PesosBultoABulto()
			END IF
			
			ls_fecha 								= 	String(dw_2.Object.opve_fecvac[1],"dd/mm/yyyy")
			ls_hora  								= 	String(dw_1.Object.opvd_horava[ll_Fila],"hh:mm:ss")
			dw_1.Object.opvd_horava[ll_Fila]	=	Time(ls_hora)
			ls_hora  								= 	String(dw_1.Object.opvd_horate[ll_Fila],"hh:mm:ss")
			dw_1.Object.opvd_horate[ll_Fila]	=	Time(ls_hora)
		END IF
		
		dw_1.Object.clie_codigo[ll_Fila]		=	Integer(istr_mant.argumento[16])
		dw_2.Object.clie_codigo[1]				=	dw_2.Object.clie_codigo[1]
		
	ELSEIF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = DataModified! THEN
		dw_2.Object.clie_codigo[1]	=	dw_2.Object.clie_codigo[1]
	END IF
NEXT
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq
String ls_Nula
SetNull(ls_nula)

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=	istr_mant.argumento[2]
lstr_busq.argum[3]	=	istr_mant.argumento[3]
lstr_busq.argum[4]	=	istr_mant.argumento[16]

OpenWithParm(w_busc_spro_ordenvaciado, lstr_busq)
lstr_busq	=	Message.PowerObjectParm

If lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" Then
	dw_2.Object.plde_codigo[1] = integer(lstr_busq.argum[1])
	dw_2.Object.orpr_tipord[1] = integer(lstr_busq.argum[2])
	dw_2.Object.opve_fecvac[1] = Date(Mid(lstr_busq.argum[4], 1, 10))
	dw_2.Object.opve_turno[1]  = integer(lstr_busq.argum[5])
	
	If iuo_ordenproceso.Existe(integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2]), &
										Integer(lstr_busq.argum[3]),True,Sqlca,Integer(istr_mant.argumento[16])) Then	
			If Not manbin_especie(integer(lstr_busq.argum[1]), iuo_ordenproceso.especie, True, sqlca) Then
				dw_2.SetItem(1,"orpr_numero", Integer(ls_Nula))
				Return
			End If
			
			If Not TieneTarjas(Integer(lstr_busq.argum[3])) Then
				dw_2.SetItem(1,"orpr_numero", Integer(ls_Nula))
				Return
			End If
			
			istr_mant.argumento[1]	= lstr_busq.argum[1]
			istr_mant.argumento[2]	= lstr_busq.argum[2]
			istr_mant.argumento[3]	= lstr_busq.argum[3]
			istr_mant.argumento[4]	= lstr_busq.argum[4]
			istr_mant.argumento[5]	= lstr_busq.argum[5]
			
			If existeencabezado(Long(istr_mant.argumento[3])) Then
				TriggerEvent("ue_recuperadatos")
			Else
				dw_2.Object.line_codigo[1] = iuo_ordenproceso.linea
				dw_2.object.orpr_fecpro[1] = iuo_ordenproceso.fechaorden
				dw_2.object.prod_codigo[1] = iuo_ordenproceso.productor
				dw_2.object.espe_codigo[1] = iuo_ordenproceso.especie
				dw_2.object.vari_codigo[1] = iuo_ordenproceso.variedad
				dw_2.Object.orpr_estado[1] = iuo_ordenproceso.estado
				dw_2.Object.clie_codigo[1] = Integer(istr_mant.argumento[16])
				
				LlenaNombres(iuo_ordenproceso.productor,iuo_ordenproceso.especie,iuo_ordenproceso.variedad,Integer(istr_mant.argumento[16]))
     		End If	
		Else
			dw_2.SetItem(1,"orpr_numero",long(ls_Nula))
		End If
End If
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date ld_fecha

istr_info.titulo	= "CONFIRMACION DE VACIADO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

IF Long(istr_mant.argumento[2]) = 9 THEN
	vinf.dw_1.DataObject = "dw_info_spro_ordenprocvacenca_com"
ELSE
	vinf.dw_1.DataObject = "dw_info_spro_ordenprocvacenca"
END IF

vinf.dw_1.SetTransObject(sqlca)

ld_Fecha	=	Date(left(istr_Mant.Argumento[4], 10))

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
								  Integer(istr_mant.Argumento[2]), &
								  Integer(istr_mant.Argumento[3]), &
								  ld_fecha, integer(istr_mant.Argumento[5]), &
								  Integer(istr_mant.Argumento[16]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0


maximo	= dw_1.width

IF dw_2.width > maximo THEN maximo = dw_2.width

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41
//dw_1.width				= This.workspacewidth() - gb_1.width - 310

dw_2.x					= 37 + Round((dw_1.width - dw_2.width) / 2, 0)
dw_2.y					= 37


end event

event ue_validaborrar;
IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	IF dw_2.Object.orpr_estado[1] > 2 THEN
		MessageBox("Error de Eliminación","La orden de proceso no se puede eliminar, ya que posee movimientos asociados.")
	  	Message.DoubleParm = -1
		RETURN
	END IF	
   IF dw_2.Object.opve_estado[1] = "C" THEN
		MessageBox("Error de Eliminación","La orden de vaciado no se puede eliminar, ya que se encuentra cerrada.")
	  	Message.DoubleParm = -1
		RETURN 
	END IF	 
	Message.DoubleParm = 1
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_ordenprocvaciado
integer x = 46
integer y = 660
integer width = 4206
integer height = 1112
string title = "Detalle de Proceso Vaciado"
string dataobject = "dw_mant_mues_spro_ordenprocvacdeta_pesos"
end type

event dw_1::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	dw_1.GetChild("enva_codigo", idwc_envase)
	idwc_envase.SetTransObject(SqlCa)
	idwc_envase.Retrieve(this.Object.enva_tipoen[il_fila])
END IF

RETURN 0
end event

event dw_1::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = False

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event dw_1::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

event dw_1::itemchanged;Integer li_lote, li_tipen, li_envas
String  ls_Columna, ls_Nula, ls_lote
Long    ll_total, ll_bultos
time    lt_fechahora

lt_fechahora=Time('00:00')

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "enva_tipoen"
		ll_bultos= dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote  = dw_1.Object.lote[il_fila]
		li_tipen = dw_1.Object.enva_tipoen[il_fila]
		li_envas = dw_1.Object.enva_codigo[il_fila]	
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
			actualizatotalbultos(ls_lote,li_tipen, li_envas,0,ll_bultos)
			
			IF NOT HabilitaBultos("enva_tipoen",data) THEN
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
			   This.SetItem(row, "enva_nombre", ls_Nula)
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			END IF	
		END IF

	CASE "enva_codigo"
		ll_bultos= dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote  = dw_1.Object.lote[il_fila]
		li_tipen = dw_1.Object.enva_tipoen[il_fila]
		li_envas = dw_1.Object.enva_codigo[il_fila]	
		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR Duplicado("enva_codigo", Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			This.SetItem(row, "enva_nombre", ls_Nula)
			RETURN 1
		ELSE
			dw_1.GetChild("cale_calida", idwc_calidad)
			idwc_calidad.SetTransObject(SqlCa)
			idwc_calidad.Retrieve(istr_Envase.TipoEnvase,integer(data))
			This.Object.enva_nombre[row]	=	istr_Envase.Nombre
 		   actualizatotalbultos(ls_lote,li_tipen, li_envas,0,ll_bultos)
			IF NOT HabilitaBultos("enva_codigo",data) THEN
				This.SetItem(row, ls_Columna, Integer(ls_Nula))
			   This.SetItem(row, "enva_nombre", ls_Nula)
				RETURN 1
			END IF	
		END IF
	
	CASE "lote"
		IF Len(Data)<10 THEN
			MessageBox("Atención","Número de Lote Ingresado Incorrectamente. Debe Ingresar 10 Dígitos.")
			This.SetItem(row, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			IF NOT HabilitaBultos("lote",data) THEN
				This.SetItem(row, ls_Columna, ls_Nula)
				RETURN 1
			END IF	
		END IF	
		
	CASE "opvd_horava"
		 IF Duplicado("opvd_horava", Data) THEN
			This.SetItem(row, ls_Columna, lt_fechahora)
			RETURN 1
		END IF
		
	CASE "opvd_canbul"
		ll_bultos = dw_1.Object.opvd_canbul[il_fila]
		IF dwo.Type = 'column' THEN
			IF Not This.uf_validate(row) THEN
				This.SetItem(row,"opvd_canbul",Integer(ls_Nula))
				RETURN 1
			END IF
		END IF
		ll_total =  dw_1.Object.bultostot[row] + ll_bultos
		IF (Long(Data) > ll_total) THEN //dw_1.Object.bultostot[row]) then  
			MessageBox("Atención","Número de Bultos Ingresado Sobrepasa a los Bultos del Movimiento.")
			This.SetItem(row, ls_Columna, ll_bultos)
			RETURN 1
		ELSE
			dw_1.Object.bultostot[row] = ll_total - Long(Data)
			actualizatotalbultos(dw_1.Object.lote[il_fila],dw_1.Object.enva_tipoen[il_fila], &
			                     dw_1.Object.enva_codigo[il_fila],il_fila,dw_1.Object.bultostot[row])
		END IF
		
   CASE "cale_calida"
		BuscaCalidad(data)
		
	CASE "lote_codigo"
		ll_bultos= dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote  = dw_1.Object.lote[il_fila]
		li_tipen = dw_1.Object.enva_tipoen[il_fila]
		li_envas = dw_1.Object.enva_codigo[il_fila]	
		IF isnull(dw_1.Object.lote_pltcod[row]) OR dw_1.Object.lote_pltcod[row]=0 THEN
			MessageBox("Atención","Primero ingrese una planta para el lote.")
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
		IF Duplicado("lote_codigo", Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		li_Lote = Integer(data)
		dw_1.Object.lote[row] = String(dw_1.Object.lote_pltcod[row],'0000') + &
										String(dw_1.Object.lote_espcod[row],'00') + &
										String(li_Lote,'0000')
		actualizatotalbultos(ls_lote,li_tipen, li_envas,0,ll_bultos)
		IF NOT HabilitaBultos("lote",dw_1.Object.lote[row]) THEN
			This.SetItem(row, ls_Columna, ls_Nula)
			RETURN 1
		END IF	
		
		
	CASE "lote_pltcod"
		ll_bultos= dw_1.Object.opvd_canbul[il_fila] + dw_1.Object.bultostot[il_fila]
		ls_lote  = dw_1.Object.lote[il_fila]
		li_tipen = dw_1.Object.enva_tipoen[il_fila]
		li_envas = dw_1.Object.enva_codigo[il_fila]	
		
		actualizatotalbultos(ls_lote,li_tipen, li_envas,0,ll_bultos)
		
		This.SetItem(row,"lote" , ls_Nula)
		This.SetItem(row,"lote_codigo",integer(ls_Nula))
		This.SetItem(row,"enva_tipoen",integer(ls_Nula))
		This.SetItem(row,"enva_codigo",integer(ls_Nula))
		This.SetItem(row,"enva_nombre" , ls_Nula)
		This.SetItem(row,"opvd_canbul" ,long(ls_Nula))
		This.SetItem(row,"bultostot" , long(ls_Nula))
		
		IF Not iuo_plantalote.existe(integer(data),True,Sqlca) OR Duplicado("lote_pltcod", Data)THEN
			This.SetItem(row,"lote" , ls_Nula)
			this.SetItem(row,"lote_codigo",integer(ls_Nula))
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF	
		
END CHOOSE
end event

event type long dw_1::dwnkey(keycode key, unsignedlong keyflags);il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_spro_ordenprocvaciado.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_spro_ordenprocvaciado.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event dw_1::getfocus;//
end event

event dw_1::buttonclicked;call super::buttonclicked;
CHOOSE CASE dwo.Name
	
	CASE "b_lotes"
       BuscaDetalleLotes() 
				

END CHOOSE
end event

event dw_1::constructor;call super::constructor;This.Uf_add_validation('opvd_canbul > 0 and opvd_canbul <= 9999','Valores fuera de rango para cantidad de bultos')
end event

event dw_1::doubleclicked;il_fila	=	row
dw_1.SetRow(il_fila)
dw_1.SelectRow(0, False)
dw_1.SelectRow(il_fila, True)

Parent.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_ordenprocvaciado
integer x = 690
integer y = 52
integer width = 2926
integer height = 536
string dataobject = "dw_mant_spro_ordenprocvacenca"
end type

event dw_2::itemchanged;Long		ll_null
Integer	li_codigo, li_null
String	ls_columna
Date     ld_fecha

SetNull(ll_null)
SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "clie_codigo"
		
			IF NoExisteCliente(Integer(Data)) THEN
				This.SetItem(row, ls_Columna, li_Null)
				RETURN 1
			END IF	
			
			istr_mant.argumento[16]	=	data
		
	CASE "orpr_numero"	
		
		IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
											Integer(istr_Mant.Argumento[2]), &
											Long(data),True,Sqlca,&
											Integer(istr_mant.argumento[16])) THEN	
											
			istr_mant.argumento[3]	= data
			
			IF Not manbin_especie(This.Object.plde_codigo[row], iuo_ordenproceso.especie, True, sqlca) THEN
				dw_2.SetItem(1,"orpr_numero",ll_Null)
				RETURN 1
			END IF
			
			IF istr_mant.argumento[2] <> '5' and istr_mant.argumento[2] <> '8'  THEN
				IF NOT gstr_paramplanta.packing THEN
					IF Not TieneTarjas(Long(Data)) THEN
						dw_2.SetItem(1,"orpr_numero",ll_Null)
						RETURN 1
					END IF
				END IF
			END IF
			
			IF existeencabezado(Long(Data)) THEN
				parent.TriggerEvent("ue_recuperadatos")
			ELSE
				dw_2.Object.line_codigo[1] = iuo_ordenproceso.linea
				dw_2.object.orpr_fecpro[1] = iuo_ordenproceso.fechaorden
				dw_2.object.prod_codigo[1] = iuo_ordenproceso.productor
				dw_2.object.espe_codigo[1] = iuo_ordenproceso.especie
				dw_2.object.vari_codigo[1] = iuo_ordenproceso.variedad
				dw_2.Object.orpr_estado[1] = iuo_ordenproceso.estado
				
				IF iuo_ordenproceso.fechaorden > &
				   dw_2.Object.opve_fecvac[1] THEN
					dw_2.Object.opve_fecvac[1] = iuo_ordenproceso.fechaorden
					istr_Mant.Argumento[4]     = String(iuo_ordenproceso.fechaorden,'dd/mm/yyyy')
				END IF	
				
				LlenaNombres(iuo_ordenproceso.productor,iuo_ordenproceso.especie,&
				             iuo_ordenproceso.variedad,Integer(istr_mant.argumento[16]))
				
     		END IF
			 dw_1.Enabled	=	True
		ELSE
			dw_2.SetItem(1,"orpr_numero",ll_Null)
			RETURN 1
		END IF
		
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		dw_2.SetItem(1,"orpr_numero",ll_Null)
		
	CASE "opve_fecvac"
		ld_fecha = date(mid(data,1,10))
		IF  ld_fecha > date(idt_fechasistema) THEN
			MessageBox("Atención","La fecha del vaciado no puede ser mayor a la fecha actual.")
			dw_2.SetItem(1,"opve_fecvac",date(idt_fechasistema))
			Return 1
		ELSEIF ld_fecha<dw_2.Object.orpr_fecpro[1] THEN
			MessageBox("Atención","La fecha del vaciado no puede ser menor a la fecha de la Orden.")
			dw_2.SetItem(1,"opve_fecvac",date(idt_fechasistema))
			Return 1
		ELSE	
			istr_Mant.Argumento[4] = String(Date(mid(data,1,10)),'dd/mm/yyyy')
			IF existeencabezado(dw_2.Object.orpr_numero[1]) THEN
				parent.TriggerEvent("ue_recuperadatos")
			END IF 
		END IF	

	
	CASE "opve_turno"
		istr_mant.argumento[5] = Data
		IF istr_mant.argumento[3] <> "" THEN
			IF iuo_ordenproceso.Existe(gstr_ParamPlanta.CodigoPlanta,&
												Integer(istr_Mant.Argumento[2]), &
												Long(istr_Mant.Argumento[3]),True,Sqlca,&
												Integer(istr_mant.argumento[16])) THEN	
										
				IF existeencabezado(long(istr_Mant.Argumento[3])) THEN
					parent.TriggerEvent("ue_recuperadatos")
				ELSE
					dw_2.Object.line_codigo[1] = iuo_ordenproceso.linea
					dw_2.object.orpr_fecpro[1] = iuo_ordenproceso.fechaorden
					dw_2.object.prod_codigo[1] = iuo_ordenproceso.productor
					dw_2.object.espe_codigo[1] = iuo_ordenproceso.especie
					dw_2.object.vari_codigo[1] = iuo_ordenproceso.variedad
					dw_2.Object.orpr_estado[1] = iuo_ordenproceso.estado
					
					LlenaNombres(iuo_ordenproceso.productor,iuo_ordenproceso.especie,&
									 iuo_ordenproceso.variedad,Integer(istr_mant.argumento[16]))
					
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

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 276
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 452
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 640
integer weight = 400
fontcharset fontcharset = ansi!
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 816
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 996
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 1384
end type

event pb_ins_det::clicked;call super::clicked;//
end event

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 1556
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_ordenprocvaciado
integer x = 4384
integer y = 92
end type

type dw_3 from datawindow within w_maed_spro_ordenprocvaciado
boolean visible = false
integer x = 210
integer y = 1184
integer width = 2784
integer height = 512
integer taborder = 30
boolean bringtotop = true
boolean titlebar = true
string title = "lotes vaciados"
string dataobject = "dw_mues_lotes_vaciados"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_spro_ordenprocvaciado
boolean visible = false
integer x = 46
integer y = 1796
integer width = 3159
integer height = 364
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_spro_ordenpesovaciado"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_spro_ordenprocvaciado
boolean visible = false
integer x = 55
integer y = 56
integer width = 567
integer height = 352
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_ordenprocvacenca"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

