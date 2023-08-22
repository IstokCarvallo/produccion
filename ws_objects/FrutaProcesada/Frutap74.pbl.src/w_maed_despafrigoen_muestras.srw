$PBExportHeader$w_maed_despafrigoen_muestras.srw
forward
global type w_maed_despafrigoen_muestras from w_mant_encab_deta_csd
end type
end forward

global type w_maed_despafrigoen_muestras from w_mant_encab_deta_csd
integer width = 3561
integer height = 2256
string title = "DESPACHO MUESTRAS LOTES INSPECCION"
string menuname = ""
boolean minbox = false
boolean maxbox = false
event ue_imprimir ( )
end type
global w_maed_despafrigoen_muestras w_maed_despafrigoen_muestras

type variables
w_mant_deta_despafrigoinfo iw_mantencion

DataWindowChild	dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes,&
						dw_ptodes,dw_sitios,idwc_patente,idwc_multipuerto,idwc_cargotecnico,idwc_especie

Transaction	sqlconec
Transaction	sqlconec2

Boolean		ib_conectado, ib_existe_folioD, ib_conectado2
Integer     	ii_tipoin, ii_secuen, ii_controlaaceso, ii_existe, ii_blockcont, il_destino, il_desreferencia 
Long        	il_numins, il_Folio
String			is_rutclie

DataStore	ids_palletfruta_fecha, ids_CorrelMovim

uo_patente				iuo_patente
uo_especie				iuo_especie
uo_clienprove			iuo_clprv
uo_ClientesProd		iuo_Cliente
uo_guiadespacho		iuo_Guia
uo_sitiorevision			iuo_Sitio
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public function boolean conexionbase ()
public function long buscanuevofolio (integer planta)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean busca_numeroinspeccion (integer cliente, integer planta, long pallet)
public subroutine existe_cargaregistro ()
public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta)
public function boolean existefolio (string as_columna, string as_valor)
public function boolean buscaregistros ()
public function boolean coneccionbase ()
public function string rescatacorreo (integer ai_planta)
public function boolean existe_pallet (integer ai_cliente, integer ai_planta, long al_pallet)
public subroutine cambiaestadopallet (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean buscasellos (long sello)
protected function boolean noexisteembarque (string as_embarque)
public function boolean buscadestino (string as_embarque, integer ai_tratamiento)
public subroutine buscacliente ()
public function boolean existe_plasag (integer ai_planta, long ai_plasag)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DESPACHO DE FRUTA PROCESADA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_despafrigoen_muestra"

vinf.dw_1.GetChild("plde_codigo", dw_plantas)
vinf.dw_1.GetChild("etiq_codigo", dw_etiquetas)
vinf.dw_1.GetChild("clie_codigo", dw_clientes)

dw_plantas.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_clientes.SetTransObject(sqlca)

dw_plantas.Retrieve(1)
dw_etiquetas.Retrieve()
dw_clientes.Retrieve()

vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("defe_tecnic", idwc_cargotecnico)
idwc_cargotecnico.SetTransObject(sqlca)
idwc_cargotecnico.Retrieve(dw_2.Object.plde_codigo[1])

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))

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

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	//Problema (Cambio de Protec  = Protect
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.defe_numero.Protect	=	0
	dw_2.Object.defe_fecdes.Protect	=	0
	dw_2.Object.defe_horade.Protect	=	0

	dw_2.Object.defe_numero.Color	= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.defe_fecdes.Color 	= 0
	dw_2.Object.defe_horade.Color 	= 0
	
	dw_2.Object.defe_numero.BackGround.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_fecdes.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_horade.BackGround.Color 	= Rgb(255,255,255)

	dw_2.SetColumn("defe_numero")
	dw_2.SetFocus()
ELSE
	//Problema (Cambio de Protec  = Protect
	dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.defe_numero.Protect	=	1
	dw_2.Object.defe_fecdes.Protect	=	1
	dw_2.Object.defe_horade.Protect	=	1

	dw_2.Object.defe_numero.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_fecdes.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_horade.Color 	= Rgb(255,255,255)
	
	dw_2.Object.defe_numero.BackGround.Color	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
	dw_2.Object.defe_fecdes.BackGround.Color 	= 553648127
	dw_2.Object.defe_horade.BackGround.Color 	= 553648127
END IF
end subroutine

public subroutine habilitaingreso (string columna);Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

If dw_2.RowCount() > 0 Then
	If IsNull(dw_2.Object.plde_codigo[1]) Or dw_2.Object.plde_codigo[1] = 0 Or &	
		IsNull(dw_2.Object.sire_codigo[1]) Or dw_2.Object.sire_codigo[1] = 0 Or &		
		IsNull(dw_2.Object.defe_fecdes[1]) Or dw_2.Object.defe_fecdes[1] = ld_fecha Or &
		IsNull(dw_2.Object.defe_cancaj[1]) Or dw_2.Object.defe_cancaj[1] = 0 Or &
		IsNull(dw_2.Object.defe_cantar[1]) Or dw_2.Object.defe_cantar[1] = 0 Or &
		IsNull(dw_2.Object.defe_especi[1]) Or dw_2.Object.defe_especi[1] = 0 Then
		lb_estado = False			
	End If
End If

If ii_blockcont = 1 Then
	If IsNull(dw_2.Object.defe_nrcont[1]) Or dw_2.Object.defe_nrcont[1] = '' Or &		
		IsNull(dw_2.Object.defe_Orcont[1]) Or dw_2.Object.defe_Orcont[1] = 0 Then
		lb_estado = False
   End If
End If
	
If dw_2.Object.defe_plasag[1]  > 0 Then
	If IsNull(dw_2.Object.defe_nturno[1]) Or dw_2.Object.defe_nturno[1] = '' Or &
		   	IsNull(dw_2.Object.defe_glosag[1]) Or dw_2.Object.defe_glosag[1] = '' Or &
	      	IsNull(dw_2.Object.defe_tecnic[1]) Or dw_2.Object.defe_tecnic[1] = 0  Or &
			IsNull(dw_2.Object.trat_codigo[1]) Or dw_2.Object.trat_codigo[1] = 0 Or &
			IsNull(dw_2.Object.defe_fectra[1]) Or dw_2.Object.defe_fectra[1] = ld_fecha Then
			lb_estado = False
	End If
Else	
	lb_estado = True
End If

If dw_2.Object.tica_codigo[1] = 2 Or dw_2.Object.tica_codigo[1] = 3 Then
	If IsNull(dw_2.Object.tran_codigo[1]) Or dw_2.Object.tran_codigo[1] = 0 Or &
		   IsNull(dw_2.Object.defe_chofer[1]) Or dw_2.Object.defe_chofer[1] = '' Or &
			IsNull(dw_2.Object.defe_chfrut[1]) Or dw_2.Object.defe_chfrut[1] = '' Or &
			IsNull(dw_2.Object.defe_patent[1]) Or dw_2.Object.defe_patent[1] = '' Or &			
	      IsNull(dw_2.Object.defe_cansel[1]) Or dw_2.Object.defe_cansel[1] = 0  Or &
			IsNull(dw_2.Object.defe_ubisel[1]) Or dw_2.Object.defe_ubisel[1] = '' Or &
			IsNull(dw_2.Object.defe_numsel[1]) Or dw_2.Object.defe_numsel[1] = ''  Then
			lb_estado = False
	End If
End If

If dw_2.Object.tica_codigo[1] = 2 Then	
	If IsNull(dw_2.Object.defe_nrcont[1]) Or dw_2.Object.defe_nrcont[1] = '' Or &		
	   IsNull(dw_2.Object.defe_tpcont[1]) Or dw_2.Object.defe_tpcont[1] = 0  Or &
		IsNull(dw_2.Object.defe_Orcont[1]) Or dw_2.Object.defe_Orcont[1] = 0 Then
		lb_estado = False
   End If
End If

If IsNull(dw_2.Object.defe_especi[1]) Or dw_2.Object.defe_especi[1] = 0 Then
	lb_estado = False
End If

If istr_mant.argumento[27] = '21' Or istr_mant.argumento[27] = '16' Then
	If IsNull(dw_2.Object.clpr_rut[1]) Or dw_2.Object.clpr_rut[1] = '' Or &
		IsNull(dw_2.Object.embq_codigo[1]) Or dw_2.Object.embq_codigo[1] = '' Then
		lb_estado = False
	End If	
End If
			
pb_grabar.Enabled	=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public function boolean conexionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta
Integer	li_Planta

DISCONNECT USING sqlconec;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

li_planta	=	dw_2.Object.defe_plades[1]

  SELECT pro.cone_nomodb,pro.cone_nomser,pro.cone_nombas,
         pro.cone_nodbms,pro.cone_nomusu,pro.cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dbo.plantadesp as pla,dbo.prodconectividad as pro  
   WHERE pla.plde_conpro = pro.cone_codigo  and  
         pla.plde_codigo = :li_planta ;

sqlconec.ServerName	=	ls_nomser
sqlconec.DataBase	   =	ls_nombas
sqlconec.Dbms			= 	ls_nodbms
sqlconec.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec;

IF sqlconec.SQLCode = 0 THEN
	ib_Conectado	=	True
ELSE
	ib_Conectado	=	False
END IF

RETURN ib_Conectado

end function

public function long buscanuevofolio (integer planta);/* Busca Folio para hacer una recepción de pallet a partir de
   un despacho de interpanta
	usando conextividad*/

Integer	li_planta
Long		ll_numero
Boolean	lb_nulo

li_planta	=	planta

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM dbo.RECFRUPROCEE_TRANS
 WHERE plde_codigo = :li_planta
 USING sqlconec;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla RecFruProcee_Trans")
ELSEIF sqlconec.SQLCode = 0 THEN
	
	 lb_nulo = IsNull(ll_numero)	
	
	 IF lb_nulo THEN
		 ll_numero=li_planta*10000
	 ELSE
		 ll_numero++
	 END IF
ELSE
	ll_numero=li_planta*10000
END IF

RETURN ll_numero
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
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
	END IF
ELSE
	IF ids_CorrelMovim.Update()	=	1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF ids_palletfruta_fecha.Update()=1 THEN		
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						ids_palletfruta_fecha.ResetUpdate()
						ids_CorrelMovim.ResetUpdate()
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
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean busca_numeroinspeccion (integer cliente, integer planta, long pallet);Date ld_fecha
Long ll_num_inspec

il_numins = 0 ; ii_tipoin = 0 ; ii_secuen = 0 ;

SELECT Max(inpd_fechai) 
  INTO :ld_fecha
  FROM dbo.inspecpaldet 	
 WHERE clie_codigo = :Cliente AND  
		 plde_codigo = :Planta  AND  
		 paen_numero = :Pallet; 
			 
SELECT max(inpe_numero), inpe_tipoin, inpe_secuen 
  INTO :il_numins, :ii_tipoin, :ii_secuen  
  FROM dbo.inspecpaldet 	AS i 
 WHERE clie_codigo = :Cliente AND  
		 plde_codigo = :Planta  AND  
		 paen_numero = :Pallet  AND
		 inpd_fechai = :ld_fecha
GROUP BY inpe_tipoin, inpe_secuen		 ; 
			 
IF il_numins > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF


end function

public subroutine existe_cargaregistro ();Long ll_Filrec, ll_Filpal, ll_Numero, ll_Existe, ll_Pallet, ll_Filinp, ll_Numinp, ll_Secuen, ll_Totfil
Integer li_Planta, li_Cliente, li_Tipo

IF dw_2.RowCount() > 0 THEN
	
	li_Planta 	= dw_2.Object.defe_plades[1]
	ll_Numero 	= dw_2.Object.defe_numero[1]
	li_Cliente 	= dw_2.Object.clie_codigo[1]
	
	SELECT rfpe_numero  
	  INTO :ll_Existe  
	  FROM dbo.recfruprocee_trans 
	 WHERE plde_codigo = :li_Planta  AND  
			 clie_codigo = :li_Cliente AND  
			 rfpe_numero = :ll_Numero   
	USING sqlconec ;
	 
	IF ll_Existe > 0 THEN		
			DELETE FROM dbo.recfruproced_trans 
   		 WHERE plde_codigo = :li_Planta AND  
                rfpe_numero = :ll_Existe  AND  
                clie_codigo = :li_Cliente 
          USING sqlconec ;	
							
			DELETE FROM dbo.recfruprocee_trans  
			 WHERE plde_codigo = :li_Planta AND  
			       rfpe_numero = :ll_Existe  AND  
				    clie_codigo = :li_Cliente 
		   USING sqlconec ;
	END IF
 
  END IF
COMMIT ;

end subroutine

public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta);/* Busca Folio para hacer un  Despacho de pallet */

Integer	li_Planta
Long		ll_Numero, ll_Inicia, ll_Termin, ll_Actual, ll_Quedan, ll_despacho
Boolean	lb_Nulo

li_Planta	=	ai_planta
ll_Numero	=	0

select max(defe_numero)
into :ll_despacho
from dbo.despafrigoen
where plde_codigo = :li_planta;
		
ids_CorrelMovim.Retrieve(li_Planta,2)
IF ids_CorrelMovim.RowCount() > 0 THEN
	ll_Inicia	=	ids_CorrelMovim.Object.como_inicia[1]
	ll_Termin	=	ids_CorrelMovim.Object.como_termin[1]
	ll_Actual	=	ids_CorrelMovim.Object.como_actual[1]
	
	IF Isnull(ll_Inicia) THEN ll_Inicia	=	0
	IF Isnull(ll_Termin) THEN ll_Termin	=	0
	IF Isnull(ll_Actual) THEN ll_Actual	=	0
		
	IF Isnull(ll_despacho) OR String(ll_despacho) = '' OR ll_despacho < ll_Inicia THEN
		ll_Actual = ll_Inicia
	ELSE
		ll_Actual=	ll_despacho
	END IF	

	IF ll_Inicia >= 0 AND ll_Termin > 0 THEN
		IF ll_Actual	=	0	THEN
			ll_Actual	=	ll_Inicia + 1
		ELSE
			ll_Actual++		
		END IF
		
		IF ll_Actual >= ll_Inicia AND	ll_Actual <= ll_Termin	THEN
		
			IF ll_Actual > ll_Termin THEN
				Messagebox("Atención","No Existen Números de Folios Disponibles de Movimiento de Despacho",exclamation!) 			
				ll_Numero	=	0
				RETURN ll_Numero
			ELSEIF ll_Actual = ll_Termin THEN
				Messagebox("Atención","Ultimo Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ll_Numero	=	ll_Actual
				
			ll_Quedan	=	(ll_Termin - ll_Actual)
			IF ll_Quedan <= 3 THEN
				Messagebox("Atención","Existen "+String(ll_Quedan)+" Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ids_CorrelMovim.Object.como_actual[1]	=	ll_Actual
		ELSE
			Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
			ll_Numero	=	0
			RETURN ll_Numero
		END IF		
		
	ELSE
		Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
		ll_Numero	=	0
		RETURN ll_Numero	
	END IF
	
ELSE
	Messagebox("Atención","No Existe Ningún Número de Movimiento de Despacho",exclamation!) 
	ll_Numero	=	0
	RETURN ll_Numero	
END IF

RETURN ll_numero
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.defe_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "defe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dbo.DESPAFRIGOEN
	WHERE	plde_codigo	=	:li_planta
	AND	defe_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	RETURN False
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	
	This.TriggerEvent("ue_recuperadatos")

	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[4]	= 	String(dw_2.Object.defe_cantar[1])
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
	ib_existe_folioD			=	True
   RETURN False
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		//istr_mant.argumento[3]	= String(li_cliente)
		ib_existe_folioD			=	False
		RETURN False
	ELSE
	   MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
		ib_existe_folioD			=	False
	   RETURN True
	END IF	
END IF





end function

public function boolean buscaregistros ();Integer  li_existe, li_planta, li_cliente
Long		ll_nfolio

//li_planta = dw_2.Object.plde_codigo[1]
li_planta = dw_2.Object.defe_plades[1]
li_cliente = dw_2.Object.clie_codigo[1]
ll_nfolio  = dw_2.Object.defe_numero[1]

SELECT	Count(*)
	INTO	:ii_existe
	FROM	dbo.RECFRUPROCEE_TRANS
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio 
	Using (sqlconec);
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RECFRUPROCEE_TRANS")
	RETURN True
END IF	
	
IF ii_existe > 0 THEN	
	
	ll_nfolio  = dw_2.Object.defe_guides[1]
	
	SELECT	Count(*)
		INTO	:ii_existe
		FROM	dbo.RECFRUPROCEE_TRANS
		WHERE	plde_codigo	=	:li_planta
		AND	rfpe_numero	=	:ll_nfolio 
		Using (sqlconec);
		
	IF ii_existe = 0 THEN
		li_existe = 0 
		ii_existe = 1
	ELSE
		li_existe = 1
	END IF	
		
END IF

IF li_existe > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF	

RETURN False


end function

public function boolean coneccionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

DISCONNECT USING sqlconec2;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

  SELECT cone_nomodb,cone_nomser,cone_nombas,
         cone_nodbms,cone_nomusu,cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dbo.prodconectividad   
   WHERE cone_codigo = 90;

sqlconec2.ServerName	=	ls_nomser
sqlconec2.DataBase	   =	ls_nombas
sqlconec2.Dbms			= 	ls_nodbms
sqlconec2.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec2;

IF sqlconec2.SQLCode = 0 THEN
	ib_Conectado2	=	True
ELSE
	ib_Conectado2	=	False
END IF

RETURN ib_Conectado2

end function

public function string rescatacorreo (integer ai_planta);String ls_correo
 
SELECT plde_correo 
 INTO :ls_correo  
 FROM dbo.plantadesp 
WHERE plde_codigo = :ai_planta
USING sqlca;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas")
ELSEIF sqlca.SQLCode = 100 THEN
	//RETURN True
ELSE
	RETURN ls_correo
END IF
end function

public function boolean existe_pallet (integer ai_cliente, integer ai_planta, long al_pallet);Long		li_existe
Integer	li_cliente, li_planta, ll_pallet

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	Using (sqlconec);

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN True
END IF	

IF li_existe = 0 THEN
	SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab_trans
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	Using (sqlconec);
END IF	

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab_trans")
	RETURN True
END IF	


IF li_existe > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF	


end function

public subroutine cambiaestadopallet (integer ai_cliente, integer ai_planta, long al_pallet);Long		li_existe
Integer	li_cliente, li_planta, ll_pallet

UPDATE dbo.palletencab SET
paen_estado = 1
WHERE clie_codigo = :ai_cliente
AND	plde_codigo = :ai_planta
AND	paen_numero = :al_pallet
Using (sqlconec);

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN
END IF	

RETURN



end subroutine

public function boolean buscasellos (long sello);Long		ll_numero, ll_desde, ll_hasta, ll_Null, ll_posici, ll_largosello, ll_sello
Boolean	lb_retorno=True
Integer	li_Planta, li_cansel, li_encuentra=0, li_haya
String	ls_Sellos, ls_Null

SetNull(ls_Null)
SetNull(ll_Null)

li_Planta	=	dw_2.Object.plde_codigo[1]
ls_Sellos	=	dw_2.Object.defe_numsel[1]
li_cansel	=	dw_2.Object.defe_cansel[1]

IF IsNull(ls_Sellos) THEN
	ls_Sellos=''
END IF

IF IsNull(li_cansel) THEN
	li_cansel=0
END IF

IF li_cansel > 0 THEN
	
	ll_largosello	=	len(ls_Sellos)
	ll_sello			=	len(String(sello))
	ll_sello			=	ll_sello+4
	
	For ll_posici = 1 To ll_largosello
		
		 IF Mid(ls_Sellos,ll_posici,1)='-' THEN li_encuentra++
	  	 IF Mid(ls_Sellos,ll_posici,ll_sello)='- '+String(sello)+' -' THEN li_haya++
			
	Next
	
	IF li_encuentra >= li_cansel THEN
			MessageBox("Error","Cantidad de Sellos Registrados Supera los Informados",Information!, Ok!)
			RETURN False
	END IF
	IF li_haya > 0 THEN
			MessageBox("Error","Número de Sello Duplicado",Information!, Ok!)
			RETURN False
	END IF


END IF

SELECT sell_inicio, sell_termin
  INTO :ll_desde, :ll_hasta
  FROM dbo.correlsellos
  WHERE plde_codigo = :li_planta
  AND   sell_vigenc = 0;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla Correlsellos")
	lb_retorno = False
ELSEIF sqlconec.SQLCode = 0 THEN
	
	 IF sello < ll_desde OR sello > ll_hasta THEN
			MessageBox("Error","Sello Fuera de Rango de Control",Information!, Ok!)
			lb_retorno = False
	 ELSE
		 ls_Sellos	=	ls_Sellos + " - " + String(sello)
		 dw_2.Object.defe_numsel[1]	=	ls_Sellos
		 dw_2.Object.sello[1]			=  ll_Null
					 
		 lb_retorno = True
	 END IF

ELSE
	lb_retorno = False
END IF

RETURN lb_retorno
end function

protected function boolean noexisteembarque (string as_embarque);String	ls_Nombre, ls_recibidor
Integer	li_Puerto, li_Cliente, li_Destino

li_Cliente	=	Integer(istr_mant.Argumento[3])

SELECT	em.embq_nomnav, em.puer_codigo, em.dest_codigo, cons_nombre
	INTO	:ls_Nombre, :li_Puerto, :li_Destino, :ls_recibidor
	FROM	dbo.embarqueprod as em, dbo.consignatario as re
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque
   AND   em.embq_clifac = re.cons_codigo;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	dw_2.SetItem(il_fila, "embq_nomnav", ls_Nombre)
	dw_2.SetItem(il_fila, "puer_codigo", li_Puerto)
	dw_2.SetItem(il_fila, "reci_nombre", ls_recibidor)
	
	istr_mant.Argumento[5]	=	as_Embarque
	istr_mant.Argumento[7]	=	String(li_Destino)
	il_destino					=	li_Destino
	
	RETURN False
END IF
end function

public function boolean buscadestino (string as_embarque, integer ai_tratamiento);Integer	li_Puerto, li_Cliente, li_Destino

li_Cliente	=	Integer(istr_mant.Argumento[3])

SELECT	em.dest_codigo
	INTO	:li_Destino
	FROM	dbo.embarqueprod as em
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	istr_mant.Argumento[5]	=	as_Embarque
	istr_mant.Argumento[7]	=	String(li_Destino)
	il_destino					=	li_Destino
	
	SELECT trat_desref
	INTO :il_desreferencia
	FROM dbo.tratamientos
	WHERE trat_codigo = :ai_tratamiento;
	
END IF
end function

public subroutine buscacliente ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_clienprove, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) < 2 THEN Return

IF lstr_busq.argum[1] = "" THEN
//	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clpr_rut[il_fila]		=	lstr_busq.argum[1]
	dw_2.Object.clpr_nombre[il_fila]	=	lstr_busq.argum[2]
	is_rutclie								=	lstr_busq.argum[1]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existe_plasag (integer ai_planta, long ai_plasag);Long		li_existe, ll_numero, ll_pallet
Integer	li_cliente, li_planta, li_cont

SELECT COUNT(*),max(defe_numero),max(clie_codigo)
	INTO :li_existe,:ll_numero,:li_cliente
	FROM dbo.despafrigoen
	WHERE plde_codigo = :ai_planta
	AND	defe_plasag = :ai_plasag;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla despafrigoen")
	RETURN True
END IF	

IF li_existe > 0 THEN
	
	li_cont = MessageBox("Existe", "Número de Planilla "+String(ai_plasag)+ " Ya Existe en Despacho Nro. "&
							+String(ll_numero)+" del Cliente "+string(li_cliente)+'.'+"~n~ Desea Continuar",Exclamation!, OKCancel!, 2)

	IF li_cont = 1 THEN
		RETURN False
	ELSE
		RETURN True
	END IF
ELSE	
	RETURN False
END IF	


end function

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
						[4]	=	Cantidad de Tarjas
						[5]	=	Código de Embarque
						[7]	=	Código de Destino
						[8]	=	Cantidad de Cajas
						[10]	=	Especie del multipuerto
						[11]	=	Planilla Sag
						[12]	=	multipuerto
						[20]	=	Tipo movimiento
						[20]	=	Guia despacho
*/

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/

  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dbo.parempresa  
	 USING sqlca;

istr_mant.argumento[8] =" "
x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("defe_plades", dw_plades)
dw_2.GetChild("puer_codigo", dw_puerto)
dw_2.GetChild("sire_codigo", dw_sitios)
dw_2.GetChild("defe_espmul", idwc_multipuerto)
dw_2.GetChild("defe_especi", idwc_especie)
dw_2.GetChild("defe_tecnic", idwc_cargotecnico)

dw_planta.SetTransObject(sqlca)
dw_plades.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)
dw_sitios.SetTransObject(sqlca)
idwc_multipuerto.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
idwc_cargotecnico.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_plades.Retrieve(1)
dw_puerto.Retrieve(-1)
dw_sitios.Retrieve(-1)
idwc_especie.Retrieve()
idwc_multipuerto.Retrieve(-1)
idwc_cargotecnico.Retrieve(gi_codplanta)

dw_2.SetItem(1, "clie_codigo",gi_codexport)
istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)
istr_mant.argumento[7]=''
istr_mant.argumento[27]='32'

pb_nuevo.PostEvent(Clicked!)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
							
sqlconec						=	CREATE Transaction
sqlconec2					=	CREATE Transaction

IF ii_ControlaAceso = 1 THEN
	IF NOT coneccionbase() THEN
		MessageBox("Sin Conexión", "No Existe Conexión a Base Control de Acceso.", StopSign!, Ok!)	
	ELSE
		dw_2.Object.defe_patent.Dddw.Name				=	'dw_mues_controlacceso'
		dw_2.Object.defe_patent.Dddw.DisplayColumn		=	'ctac_patent'
		dw_2.Object.defe_patent.Dddw.DataColumn		=	'ctac_patent'
		dw_2.Object.defe_patent.Dddw.AllowEdit			=  True
		dw_2.Object.defe_patent.Dddw.HScrollBar			=  True
		dw_2.Object.defe_patent.Dddw.VScrollBar			=  True
		dw_2.Object.defe_patent.Dddw.Case 				= 	"Upper"
		dw_2.Modify("defe_patent.dddw.Limit=10")
		dw_2.Modify("defe_patent.Dddw.PercentWidth=250")
	
		dw_2.GetChild("defe_patent", idwc_patente)
		idwc_patente.SetTransObject(sqlconec2)
		idwc_patente.Retrieve()
	END IF
END IF

iuo_patente	=	Create uo_patente
iuo_Especie	=	Create uo_Especie
iuo_clprv		=	Create uo_clienprove
iuo_Cliente	=	Create uo_ClientesProd
iuo_Guia		=	Create uo_guiadespacho
iuo_Sitio		=	Create uo_sitiorevision

// Para guardar fecha de despacho en la Palletfruta
ids_palletfruta_fecha					=	CREATE	DataStore
ids_palletfruta_fecha.DataObject	=	'dw_mues_palletfruta_pafrfecdes'
ids_palletfruta_fecha.SetTransObject(sqlca)

ids_CorrelMovim							=	CREATE	DataStore
ids_CorrelMovim.DataObject			=	'dw_mues_correlmoviemientos_despa'
ids_CorrelMovim.SetTransObject(sqlca)
end event

event ue_borra_detalle;call super::ue_borra_detalle;SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

Integer	li_cajas

istr_mant.argumento[10] = String(dw_2.Object.defe_especi[1])
istr_mant.argumento[11] = String(dw_2.Object.defe_plasag[1])
istr_mant.argumento[12] = String(dw_2.Object.defe_espmul[1])
istr_mant.argumento[20] = String(dw_2.Object.defe_tiposa[1])
istr_mant.argumento[25] = String(dw_2.Object.defe_guides[1])

IF dw_1.RowCount() > 0 THEN
	li_cajas	=	dw_1.Object.totalcajas[1]
	IF dw_1.Object.totalcajas[1] >= Integer(istr_mant.Argumento[8]) THEN
		MessageBox("Atención", "Se ha completado la cantidad de Cajas, No podrá ingresar más Cajas.")
		RETURN
	END IF
END IF

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), &
										Integer(istr_mant.argumento[1]), &
										Long(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	= True
				
				dw_2.SetTabOrder("clie_codigo",0)
				dw_2.SetTabOrder("plde_codigo",0)
				
				IF dw_2.Object.defe_estado[1]	=	1	THEN
					istr_mant.solo_consulta	=	True
				ELSE
					istr_mant.solo_consulta 	=	False
					pb_eli_det.Enabled		=	True
					pb_ins_det.Enabled		=	True
					pb_grabar.Enabled			=	True
					pb_eliminar.Enabled		=	True
				END IF
								
				IF ll_fila_d > 0 THEN
					pb_imprimir.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					IF 	dw_2.Object.defe_estado[1]	=	1 THEN pb_ins_det.Enabled	=	True							
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

on w_maed_despafrigoen_muestras.create
call super::create
end on

on w_maed_despafrigoen_muestras.destroy
call super::destroy
end on

event ue_nuevo;HabilitaEncab(True)


ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False

END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled		= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetTabOrder("clie_codigo",1)
dw_2.SetTabOrder("plde_codigo",2)

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.SetItem(1,"defe_horade", Now())

iuo_Cliente.Existe(gi_CodExport, False, Sqlca) 
dw_2.Object.defe_guiaem[1] = 0

dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_despafrigoen_muestra, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[5]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	ib_existe_folioD			=	True
	This.TriggerEvent("ue_recuperadatos")
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long 		ll_filas, ll_fila_d,ll_fila_g, ll_nuevofolio, li_fillas, ll_destino, ll_fila
Integer	li_control

ids_palletfruta_fecha.Reset() 

IF Not ib_existe_folioD	THEN
	IF isnull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 THEN
		ll_nuevofolio=BusNuevoFolioDespa(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
		il_Folio	=	ll_nuevofolio
		
		IF il_Folio > 0 THEN
			dw_2.Object.defe_numero[1]	= ll_nuevofolio
			dw_2.SetItem(1, "defe_numero",ll_nuevofolio)
		
			istr_mant.argumento[2]	= String(ll_nuevofolio)
			
			FOR li_fillas = 1 TO dw_1.RowCount()
				 dw_1.Object.defe_numero[li_fillas]	= ll_nuevofolio
			NEXT		
		END IF
	END IF	
ELSE
	il_Folio	=	dw_2.Object.defe_numero[1]
END IF

FOR ll_fila = 1 TO dw_1.Rowcount()
	dw_1.Object.defi_secuen[ll_fila] = ll_fila
NEXT	
	


end event

event ue_guardar;Long		ll_Guia
Integer li_codigo

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 OR il_Folio	=	0 THEN RETURN

If gi_Emisor_Electronico = 1 Then //And iuo_Cliente.Guia_Electronica = 1  Then
	If dw_2.Object.defe_guiaem[1] = 0 Then 
		ll_Guia = iuo_Guia.of_Emiteguia(dw_2.Object.plde_codigo[1], dw_2.Object.clie_codigo[1], 2)
		If ll_Guia > 0 Then
			dw_2.Object.defe_guides[1] = ll_Guia
			dw_2.Object.defe_guiaem[1] = 3
		Else
			MessageBox('Alerta', 'No se pudo obtener numero de guias de despacho.', Exclamation!, OK!)
			Message.DoubleParm = -1
		End If
	End If
End If

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	//Envia correo de despacho interplanta.
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF 
	

end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF dw_2.Object.defe_estado[1]	<>	1	THEN	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_despafrigoen_muestras
integer x = 425
integer y = 1364
integer width = 2176
integer taborder = 100
string title = "Detalle de Cajas y Kilos"
string dataobject = "dw_mues_despafrigode_muestra"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_despafrigoen_muestras
integer x = 59
integer y = 16
integer width = 2907
integer height = 1332
string dataobject = "dw_mant_despafrigoen_muestra"
boolean livescroll = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_Planilla, ll_tpcont, ll_orcont, ll_guides, ll_sello
String		ls_columna, ls_null, ls_embq_codigo, ls_TipoPla, ls_Rut, &
			ls_glosa,ls_glosag,ls_ubisel,ls_numsel,ls_nrcont,ls_patent,ls_pataco, ls_chofer, ls_rutchofer
Date		ld_nula
Integer	li_Cliente, li_Planta, li_existe, li_espmul, li_sicodigo, li_trancodigo, li_ticacodigo

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

Choose Case ls_columna
	Case 'sire_codigo'
		If Not iuo_Sitio.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		End If
		
	Case "clie_codigo"
		istr_mant.argumento[3]	= data
		If F_ValidaCliente(Integer(data)) = False Then
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			Return 1
		Else
			iuo_Cliente.Existe(Long(Data), False, Sqlca)
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			dw_2.SetItem(1, "embq_nomnav", ls_null)
			dw_2.SetItem(1, "embq_codigo", ls_null)
			
			dw_2.GetChild("defe_plades", dw_plades)
			dw_plades.SetTransObject(sqlca)
			dw_plades.Retrieve(1)
		End If
					
	Case "plde_codigo"
		ExisteFolio(ls_columna, data)
		dw_2.GetChild("defe_plades", dw_plades)
		dw_plades.SetTransObject(sqlca)
		dw_plades.Retrieve(1)
		
		dw_2.GetChild("defe_tecnic", idwc_cargotecnico)
		idwc_cargotecnico.SetTransObject(sqlca)
		idwc_cargotecnico.Retrieve(Integer(data))		
		
	Case "defe_numero"
		//ExisteFolio(ls_columna, data)
		If ExisteFolio(ls_columna, data) Then
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		End If
		
	Case "defe_cantar"
		istr_mant.argumento[4]	= data
		
	Case "defe_cancaj"
		istr_mant.argumento[8]	= data
		
	Case "embq_codigo"
		If NoExisteEmbarque(data) Then
			This.Object.embq_nomnav[row]	=	""
			This.Object.embq_codigo[row]	=	ls_null
			Return 1
		End If

	Case "defe_fecdes"
		If Not f_validafechatempo(date(data)) Then
			This.SetItem(Row, ls_Columna, ld_nula)
			Return 1
		End If	
	
Case "tica_codigo"
	If Integer(Data) = 2 Then
		ii_blockcont = 1 
	Else
		This.SetItem(1, 'defe_nrcont', (ls_null))
		This.SetItem(1, 'defe_orcont', Integer(ls_null))
		ii_blockcont = 0
	End If	
	
Case "defe_patent"
		If ib_conectado2 = True Then
			If iuo_patente.existe(data,True,sqlconec2) Then
				dw_2.Object.defe_fecing[Row] = iuo_patente.FechaIng
				dw_2.Object.defe_horing[Row] = iuo_patente.HoraIng
				dw_2.Object.defe_sucuco[Row] = iuo_patente.Sucursal
			Else	
				dw_2.Object.defe_fecing[Row] = Date(ls_null)
				dw_2.Object.defe_horing[Row] = Time(ls_null)
				dw_2.Object.defe_sucuco[Row] = Integer(ls_null)
			End If	
		End If	
	
Case "defe_especi"
	If iuo_Especie.Existe(Integer(data),True,SQLCA) Then
		istr_mant.argumento[10] = String(Data)	
		If idwc_multipuerto.Retrieve(Integer(data)) = 0 Then
			idwc_multipuerto.InsertRow(0)
		End If
	Else
		This.SetItem(row,'defe_especi',Integer(ls_null))
		Return 1
	End If
	
	Case "defe_nrcont"
		ii_blockcont = 1
		
	Case "defe_orcont"
		ii_blockcont = 1
	
	Case "sello"		
		ll_sello	=	Long(data)
		
		If busCasellos(ll_sello) = False Then
			This.SetItem(Row, ls_Columna, ll_null)
			Return 1
		End If
		
	Case "trat_codigo"
		If dw_2.Object.defe_tiposa[1] = 7 OR dw_2.Object.defe_tiposa[1] = 8 OR dw_2.Object.defe_tiposa[1] = 9 Then
			If NOT isnull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] <> '' Then
				buscadestino(dw_2.Object.embq_codigo[1],Integer(data))
			Else	
				MessageBox("Atención","Falta Selección del Embarque.")
				This.SetItem(Row, ls_Columna, Integer(ll_null))
				Return 1
			End If
			
			If Integer(il_desreferencia) <> il_destino Then
				MessageBox("Atención","Destino Seleccionado no Corresponde al Embarque.")
			End If
		End If
		
	Case "clpr_rut"			
		is_rutclie = F_VerRut(data, True)
		If is_rutclie = "" Then
			dw_2.SetItem(1, "clpr_rut", string(ll_null))
			Return 1
		Else
			If NOT iuo_clprv.existe(is_rutclie, true, sqlca) Then
				dw_2.SetItem(1, "clpr_rut", string(ll_null))
				Return 1
			Else
				dw_2.SetItem(1, "clpr_rut", is_rutclie)
				dw_2.SetItem(1, "clpr_nombre", iuo_clprv.RazonSocial)
			End If
		End If
		
	Case 'defe_chfrut'
		ls_Rut = F_VerRut(data, True)
		If ls_Rut = "" Then
			dw_2.SetItem(Row, ls_Columna, ls_Null)
			Return 1
		Else
			dw_2.SetItem(Row, ls_Columna, ls_Rut)
			Return 1
		End If
					
End Choose

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
		
	CASE "buscacliente"
		buscacliente()
   		//HabilitaIngreso('clie_codigo')
	CASE "b_termografo"
		
		IF isnull(dw_2.Object.defe_term01[row]) THEN
			istr_mant3.argumento[1] = ''
		ELSE
			istr_mant3.argumento[1] = dw_2.Object.defe_term01[row] 
		END IF
		
		IF isnull(dw_2.Object.defe_term02[row] ) THEN
			istr_mant3.argumento[2] = ''
		ELSE
			istr_mant3.argumento[2] = dw_2.Object.defe_term02[row] 
		END IF	
		
		IF  isnull(dw_2.Object.defe_term03[row]) THEN 
			istr_mant3.argumento[3] = ''	
		ELSE
			istr_mant3.argumento[3] = dw_2.Object.defe_term03[row] 
		END IF
		
		OpenWithParm(w_incluye_termografos, istr_mant3)
		istr_mant3 = Message.PowerObjectParm		
			
		dw_2.Object.defe_term01[row] = istr_mant3.argumento[1]
		dw_2.Object.defe_term02[row] = istr_mant3.argumento[2]
		dw_2.Object.defe_term03[row] = istr_mant3.argumento[3]
			
	
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 264
end type

event pb_nuevo::clicked;call super::clicked;ib_existe_folioD	=	False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 492
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 708
end type

event pb_grabar::clicked;IF Integer(dw_1.Object.totalcajas[1]) > Integer(istr_mant.argumento[8])  THEN
	MessageBox("Atención", "Suma Cajas Detalle NO Pueder Ser Mayor a Encabezado.", &
	Exclamation!, OK!)
	Return 1
END IF

Parent.TriggerEvent("ue_guardar")


end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 940
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 1168
end type

event pb_salir::clicked;DISCONNECT USING sqlconec2;

Close(Parent)
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 1452
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 1628
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_despafrigoen_muestras
integer x = 3118
integer y = 84
end type

