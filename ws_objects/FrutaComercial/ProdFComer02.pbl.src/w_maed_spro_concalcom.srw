$PBExportHeader$w_maed_spro_concalcom.srw
forward
global type w_maed_spro_concalcom from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_spro_concalcom
end type
type tp_1 from userobject within tab_1
end type
type dw_daños from datawindow within tp_1
end type
type dw_lotes from datawindow within tp_1
end type
type tp_1 from userobject within tab_1
dw_daños dw_daños
dw_lotes dw_lotes
end type
type tp_3 from userobject within tab_1
end type
type dw_madurez from datawindow within tp_3
end type
type tp_3 from userobject within tab_1
dw_madurez dw_madurez
end type
type tab_1 from tab within w_maed_spro_concalcom
tp_1 tp_1
tp_3 tp_3
end type
end forward

global type w_maed_spro_concalcom from w_mant_encab_deta_csd
integer width = 3625
integer height = 2116
string title = "Control de Calidad - Revisión"
string menuname = ""
tab_1 tab_1
end type
global w_maed_spro_concalcom w_maed_spro_concalcom

type variables
DataWindowChild	idwc_planta,idwc_productor,idwc_especie,idwc_variedad,&
						idwc_linea
						
DataWindow			dw_3,dw_4,dw_6

uo_especie			iuo_Especie
uo_Productores		iuo_Productor
uo_plantadesp		iuo_planta
uo_lineapacking   iuo_linea
uo_variedades		iuo_variedad


Long il_filadw3, il_filadw4, il_filadw6
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean existeorden (integer ai_tipo, long al_orden)
public function boolean duplicadocalibre (string as_calibre)
public function boolean existefolio (long al_folio)
protected function integer wf_modifica ()
public function boolean existelote (long al_lote)
public function boolean duplicadolote (string as_lote)
public subroutine buscalote ()
public subroutine buscadanos (integer ai_planta, integer ai_especi, long al_fclote)
public subroutine buscaorden ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaingreso (string as_columna)
end prototypes

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	
	dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.ccco_folio.Protect				=	0
	dw_2.Object.ccco_folio.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.ccco_fecha.Protect				=	0
	dw_2.Object.ccco_fecha.BackGround.Color	=	RGB(255,255,255)

	dw_2.Object.ccco_tipdoc.Protect				=	0
	dw_2.Object.ccco_tipdoc.BackGround.Color	=	RGB(255,255,255)

	dw_2.Object.ccco_docrel.Protect				=	0
	dw_2.Object.ccco_docrel.BackGround.Color	=	RGB(255,255,255)

	dw_2.Object.prod_codigo.Protect				=	0
	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.line_codigo.Protect				=	0
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(255,255,255)	

	dw_2.Object.ccco_nturno.Protect				=	0
	dw_2.Object.ccco_nturno.BackGround.Color	=	RGB(255,255,255)	
	
	dw_2.Object.b_orden.visible					=	1
ELSE
	
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.plde_codigo.BackGround.Color	=	rgb(166,180,210)
	
	dw_2.Object.ccco_folio.Protect				=	1
	dw_2.Object.ccco_folio.BackGround.Color	=	rgb(166,180,210)
	
	dw_2.Object.ccco_fecha.Protect				=	1
	dw_2.Object.ccco_fecha.BackGround.Color	=	rgb(166,180,210)

	dw_2.Object.ccco_tipdoc.Protect				=	1
	dw_2.Object.ccco_tipdoc.BackGround.Color	=	rgb(166,180,210)

	dw_2.Object.ccco_docrel.Protect				=	1
	dw_2.Object.ccco_docrel.BackGround.Color	=	rgb(166,180,210)

	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.prod_codigo.BackGround.Color	=	rgb(166,180,210)
	
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.espe_codigo.BackGround.Color	=	rgb(166,180,210)
	
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.vari_codigo.BackGround.Color	=	rgb(166,180,210)
	
	dw_2.Object.line_codigo.Protect				=	1
	dw_2.Object.line_codigo.BackGround.Color	=	rgb(166,180,210)	

	dw_2.Object.ccco_nturno.Protect				=	1
	dw_2.Object.ccco_nturno.BackGround.Color	=	rgb(166,180,210)
	
	dw_2.Object.b_orden.visible					=	0
END IF

end subroutine

public function boolean existeorden (integer ai_tipo, long al_orden);Integer	li_planta, li_especie, li_variedad
Long     ll_productor
Boolean	lb_Retorno = True

li_planta = dw_2.Object.plde_codigo[1]

 SELECT	prod_codigo, espe_codigo, vari_codigo
	INTO	:ll_productor, :li_especie, :li_variedad
	FROM	dba.spro_ordenproceso
	WHERE	plde_codigo	=	:li_Planta
	AND	orpr_tipord	=	:ai_Tipo
	AND	orpr_numero	=	:al_orden;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Proceso")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
ELSE
	dw_2.SetItem(1,"prod_codigo",ll_productor)
	dw_2.SetItem(1,"espe_codigo",li_especie)
	dw_2.GetChild("vari_codigo",idwc_variedad)
	idwc_variedad.SetTransObject(SQLCA)
	IF idwc_variedad.Retrieve(li_especie) = 0 THEN
		idwc_variedad.insertrow(0)
	ELSE	
	    dw_2.SetItem(1,"vari_codigo",li_variedad)
	END IF 
	IF ai_Tipo = 4 THEN
		dw_2.Object.prod_codigo.Protect = 1
		dw_2.Object.espe_codigo.Protect = 1
		dw_2.Object.vari_codigo.Protect = 1
		dw_2.Object.prod_codigo.BackGround.Color = RGB(166,180,210)
		dw_2.Object.espe_codigo.BackGround.Color = RGB(166,180,210)
		dw_2.Object.vari_codigo.BackGround.Color = RGB(166,180,210)
	END IF	
END IF

RETURN lb_Retorno

end function

public function boolean duplicadocalibre (string as_calibre);Long		ll_fila

ll_fila = dw_6.Find("cccm_gcalib = '" + as_calibre + "'", 1, dw_6.RowCount())		

IF ll_fila > 0 and ll_fila <> il_filadw6 THEN
	MessageBox("Error","Registro de Calibre ya fue ingresado anteriormente.", Information!, OK!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existefolio (long al_folio);Integer li_planta, li_especie
Boolean	lb_Retorno = True

li_planta = dw_2.Object.plde_codigo[1]

 SELECT	espe_codigo
	INTO	:li_especie
	FROM	dba.spro_concalcomenca
	WHERE	plde_codigo	=	:li_Planta
	AND	ccco_folio	=	:al_folio;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Control Calidad Comercial")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	lb_Retorno	=	False
ELSEIF SQLCA.SQLCODE = 0 THEN	
	IF li_especie<>0 and isnull(li_especie)=False THEN
		dw_2.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(SQLCA)
		IF idwc_variedad.Retrieve(li_especie) = 0 THEN
	   	idwc_variedad.InsertRow(0)
		END IF
	END IF	
END IF

RETURN lb_Retorno

end function

protected function integer wf_modifica ();IF dw_3.AcceptText() = -1 THEN RETURN -1
IF dw_4.AcceptText() = -1 THEN RETURN -1
IF dw_6.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1

IF (dw_3.ModifiedCount() + dw_3.DeletedCount()) > 0 THEN RETURN 0
IF (dw_4.ModifiedCount() + dw_4.DeletedCount()) > 0 THEN RETURN 0
IF (dw_6.ModifiedCount() + dw_6.DeletedCount()) > 0 THEN RETURN 0

IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public function boolean existelote (long al_lote);long		ll_lote
Integer  li_productor, li_variedad, li_planta, li_especie
Boolean	lb_Retorno = True

IF al_lote<>0 THEN
	
	li_planta 		=	dw_2.Object.plde_codigo[1]
	li_especie     =  dw_2.Object.espe_codigo[1]
	li_productor 	=	dw_2.Object.prod_codigo[1]
	li_variedad  	=	dw_2.Object.vari_codigo[1]

	 SELECT	lote_codigo
		INTO	:ll_lote
		FROM	dba.spro_lotesfrutagranel
		WHERE	lote_pltcod	=	:li_Planta
		AND	lote_espcod	=	:li_especie
		AND	lote_codigo	=	:al_lote
		AND   prod_codigo =  :li_productor
		AND   vari_codigo =  :li_variedad;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Comercial Detalle")
		
		lb_Retorno	=	FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
		lb_Retorno	=	FALSE
	
	END IF
	
END IF

RETURN lb_Retorno

end function

public function boolean duplicadolote (string as_lote);Long		ll_fila

ll_fila = dw_3.Find("foliolote = '" + as_lote + "'" , 1, dw_3.RowCount())		

IF ll_fila > 0 and ll_fila <> il_filadw3 THEN
	MessageBox("Error","Registro de Lote ya fue ingresado anteriormente.", Information!, OK!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscalote ();String ls_lote
Long   ll_fila
Str_busqueda	lstr_busq

lstr_busq.argum[1] = String(dw_2.Object.plde_codigo[1])
lstr_busq.argum[2] = String(dw_2.Object.espe_codigo[1])
lstr_busq.argum[3] = "comercial"
lstr_busq.argum[4] = String(dw_2.Object.vari_codigo[1])
lstr_busq.argum[5] = String(dw_2.Object.prod_codigo[1])

OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	dw_3.SetFocus()
ELSE
	ls_lote = String(dw_2.Object.plde_codigo[1],'0000') +  String(dw_2.Object.espe_codigo[1],'00') + &
	          String(long(lstr_busq.argum[3]),'0000')
	IF NOT Duplicadolote(ls_lote) THEN
					 
		dw_3.Object.plde_codigo[il_filadw3]	=	Integer(lstr_busq.argum[1])
		dw_3.Object.lote_espcod[il_filadw3]	=	integer(lstr_busq.argum[2])
		dw_3.Object.lote_codigo[il_filadw3]	=	long(lstr_busq.argum[3])
		ls_lote = string(dw_3.Object.plde_codigo[il_filadw3],'0000') + String(dw_3.Object.lote_espcod[il_filadw3],'00')+ &
						String(dw_3.Object.lote_codigo[il_filadw3],'0000')

		dw_3.Object.foliolote[il_filadw3]	=  ls_lote
	
		
	   IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
			Buscadanos(Integer(lstr_busq.argum[1]),integer(lstr_busq.argum[2]), &
	   	        long(lstr_busq.argum[3]))
		ELSE			  
			FOR ll_fila=1 TO dw_4.RowCount()
				dw_4.Object.lote_codigo[ll_fila] = long(lstr_busq.argum[3])
			NEXT	
		END IF			  
	END IF
END IF

RETURN
end subroutine

public subroutine buscadanos (integer ai_planta, integer ai_especi, long al_fclote); Integer li_especie, li_variedad, li_tipodd, li_dadeco, li_dadese
 String  ls_dadeno
 
li_especie  = dw_2.Object.espe_codigo[1]
li_variedad = dw_2.Object.vari_codigo[1]

SELECT vari_codigo into :li_variedad
  FROM dba.spro_danosydefectos
 WHERE espe_codigo = :li_especie
   AND vari_codigo = :li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Control Calidad Comercial")
	setnull(li_variedad)
ELSEIF sqlca.SQLCode = 100 THEN
	setnull(li_variedad)
END IF	

 
 DECLARE Buscadanos CURSOR FOR  
  SELECT dba.spro_danosydefectos.dade_tipodd,   
         dba.spro_danosydefectos.dade_codigo,   
         dba.spro_danosydefectos.dade_secuen,   
         dba.spro_danosydefectos.dade_nombre  
    FROM dba.spro_danosydefectos  
   WHERE ( dba.spro_danosydefectos.espe_codigo = :li_especie ) AND  
         ( dba.spro_danosydefectos.vari_codigo = :li_variedad OR
			  isnull(:li_variedad,0) = 0  );
			
open Buscadanos;

FETCH Buscadanos INTO :li_tipodd, :li_dadeco, :li_dadese, :ls_dadeno;
Do While SQLCA.SQlCODE = 0
	
	il_filadw4 = dw_4.Insertrow(0)
	
	dw_4.SetItem(il_filadw4,"plde_codigo",ai_planta)
	dw_4.Setitem(il_filadw4,"lote_espcod",ai_especi)
	dw_4.Setitem(il_filadw4,"lote_codigo",al_fclote)
	dw_4.Setitem(il_filadw4,"dade_tipodd",li_tipodd)
	dw_4.Setitem(il_filadw4,"dade_codigo",li_dadeco)
	dw_4.Setitem(il_filadw4,"dade_secuen",li_dadese)
	dw_4.Setitem(il_filadw4,"dade_nombre",ls_dadeno)
	
	FETCH Buscadanos INTO :li_tipodd, :li_dadeco, :li_dadese, :ls_dadeno;
LOOP	

Close Buscadanos;

dw_4.SetRedraw(FALSE)
dw_4.SetFilter("plde_codigo = " + String(ai_planta) + " AND " + &
               "lote_espcod = " + String(ai_especi) + " AND " + &
					"lote_codigo = " + String(al_fclote))

dw_4.Filter()					
dw_4.SetRedraw(TRUE)

dw_3.Object.nuevo[il_filadw3] = 1


end subroutine

public subroutine buscaorden ();Str_busqueda	lstr_busq

IF dw_2.Object.ccco_tipdoc[1] = 4 THEN
 	lstr_busq.argum[6] = "Proceso"
ELSEIF dw_2.Object.ccco_tipdoc[1] = 5 THEN
	lstr_busq.argum[6] = "Re - Proceso"
ELSE
	lstr_busq.argum[6] = ""
END IF	

OpenWithParm(w_busqueda_ordenproceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("ccco_docrel")
	dw_2.SetFocus()
ELSE
	dw_2.Object.plde_codigo[1]	=	Integer(lstr_busq.argum[1])
	dw_2.Object.ccco_tipdoc[1]	=	integer(lstr_busq.argum[2])
	dw_2.Object.ccco_docrel[1]	=	long(lstr_busq.argum[3])
	dw_2.Object.espe_codigo[1]	=	integer(lstr_busq.argum[4])
	dw_2.Object.prod_codigo[1]	=	Long(lstr_busq.argum[7])
	dw_2.Getchild("vari_codigo",idwc_variedad)
	idwc_variedad.SetTransObject(SQLCA)
	idwc_variedad.Retrieve(integer(lstr_busq.argum[4]))
	dw_2.Object.vari_codigo[1]	=	integer(lstr_busq.argum[8])
	
	IF integer(lstr_busq.argum[2]) = 4 THEN
		dw_2.Object.prod_codigo.Protect = 1
		dw_2.Object.espe_codigo.Protect = 1
		dw_2.Object.vari_codigo.Protect = 1
		dw_2.Object.prod_codigo.BackGround.Color = RGB(166,180,210)
		dw_2.Object.espe_codigo.BackGround.Color = RGB(166,180,210)
		dw_2.Object.vari_codigo.BackGround.Color = RGB(166,180,210)
	END IF	
	
	dw_2.SetFocus()
END IF

RETURN
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno, lb_Autocommit

lb_Autocommit		=	sqlca.Autocommit
sqlca.Autocommit	=	False

IF Not dw_2.uf_check_required(0) THEN RETURN False

//IF Not dw_1.uf_validate(0) THEN RETURN False

IF Borrando THEN
	IF dw_6.Update(True, False) = 1 THEN 					//Daños por Lotes
		IF dw_3.Update(True, False) = 1 THEN				//Fruta Embalada
			IF dw_4.Update(True, False) = 1 THEN			//Madurez
				IF dw_2.Update(True,False) = 1 THEN			//Encabezado
					Commit;
							
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					ELSE
						lb_Retorno	=	True
								
						dw_6.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
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
ELSE
	IF dw_2.Update(True, False) = 1 THEN		 				//Encabezado
		IF dw_4.Update(True, False) = 1 THEN					//Daños por Lotes
			IF dw_3.Update(True, False) = 1 THEN			//Emblables
				IF dw_6.Update(True,False) = 1 THEN		//Madurez
			
					Commit;
						
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
									
						dw_6.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
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
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha

IF as_Columna <> "plde_codigo" AND &
	(dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ccco_folio" AND &
	(dw_2.Object.ccco_folio[1] = 0 OR IsNull(dw_2.Object.ccco_folio[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "ccco_fecha" AND &
	(dw_2.Object.ccco_fecha[1] = ld_Fecha OR IsNull(dw_2.Object.ccco_fecha[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "ccco_tipdoc" AND &
	(dw_2.Object.ccco_tipdoc[1] = 0 OR IsNull(dw_2.Object.ccco_tipdoc[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "ccco_docrel" AND &
	(dw_2.Object.ccco_docrel[1] = 0 OR IsNull(dw_2.Object.ccco_docrel[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "prod_codigo" AND &
	(dw_2.Object.prod_codigo[1] = 0 OR IsNull(dw_2.Object.prod_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "espe_codigo" AND &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "vari_codigo" AND &
	(dw_2.Object.vari_codigo[1] = 0 OR IsNull(dw_2.Object.vari_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "line_codigo" AND &
	(dw_2.Object.line_codigo[1] = 0 OR IsNull(dw_2.Object.line_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ccco_nturno" AND &
	(dw_2.Object.ccco_nturno[1] = 0 OR IsNull(dw_2.Object.ccco_nturno[1])) THEN
	lb_Estado = False
END IF


tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_3.Enabled	=	lb_Estado

pb_ins_det.Enabled	=	lb_Estado
pb_grabar.Enabled		=	lb_Estado

IF lb_estado THEN
	Habilitaencab(FALSE)
END IF	
end subroutine

on w_maed_spro_concalcom.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_spro_concalcom.destroy
call super::destroy
destroy(this.tab_1)
end on

event open;/* Argumentos
 istr_mant.argumento[1] = Código Planta.
 istr_mant.argumento[2] = Folio.
 istr_mant.argumento[3] = Tipo Documento.
 istr_mant.argumento[4] = Número de Proceso.
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2]	=	""
istr_mant.argumento[3]	=	""
istr_mant.argumento[4]	=	""


dw_3	=	tab_1.tp_1.dw_lotes
dw_4	=	tab_1.tp_1.dw_daños
//dw_5	=	tab_1.tp_2.dw_frutos
dw_6	=	tab_1.tp_3.dw_madurez

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()

dw_2.GetChild("prod_codigo",idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve()

dw_2.GetChild("espe_codigo",idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve(gstr_parempresa.empr_codexp)

dw_2.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.insertrow(0)

dw_2.GetChild("line_codigo",idwc_linea)
idwc_linea.SetTransObject(SQLCA)
idwc_linea.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
//dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)


dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 84")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.Modify("DataWindow.Footer.Height = 84")

//dw_5.Modify("datawindow.message.title='Error '+ is_titulo")
//dw_5.Modify("DataWindow.Footer.Height = 84")

dw_6.Modify("datawindow.message.title='Error '+ is_titulo")
dw_6.Modify("DataWindow.Footer.Height = 84")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)


iuo_Especie			=	Create uo_especie
iuo_Productor		=	Create uo_Productores	
iuo_planta			=	Create uo_plantadesp		
iuo_linea			=	Create uo_lineapacking   
iuo_variedad		=	Create uo_variedades		

end event

event ue_nuevo();call super::ue_nuevo;Long		ll_modif

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_4.GetNextModified(0, Primary!)
			ll_modif	+=	dw_6.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			
			IF dw_3.RowCount() > 0 and ll_modif > 0 THEN
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

dw_3.Reset()
dw_4.Reset()
dw_6.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False

dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta

istr_Mant.Argumento[1]			=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.Argumento[2]			=	""
istr_mant.Argumento[3]			=	""
istr_mant.Argumento[4]			=	""
		
tab_1.tp_1.Enabled	=	False
tab_1.tp_3.Enabled	=	False

pb_ins_det.Enabled	=	False
pb_eli_det.Enabled	=	False

tab_1.SelectTab(1)

HabilitaEncab(True)	

dw_2.SetFocus()
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila_e, respuesta, ll_lotefc, ll_fila
Integer li_planta, li_especi
String ls_lote

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),&
										 long(istr_mant.argumento[2]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
		
		tab_1.tp_1.Enabled		=	True
		tab_1.tp_3.Enabled		=	True

		HabilitaEncab(False)

		DO
			IF dw_3.Retrieve(Integer(istr_mant.argumento[1]),&
							     long(istr_mant.argumento[2])) = -1 OR &
				dw_4.Retrieve(Integer(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2])) = -1 OR &
				dw_6.Retrieve(Integer(istr_mant.argumento[1]),&
							     Long(istr_mant.argumento[2])) = -1  THEN
								  
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)

			ELSE
				
				FOR ll_fila=1 To dw_3.RowCount()
					li_planta = dw_3.Object.plde_codigo[ll_fila]
					li_especi = dw_3.Object.lote_espcod[ll_fila]
					ll_lotefc = dw_3.Object.lote_codigo[ll_fila]
					ls_lote   = String(li_planta,'0000') + String(li_especi,'00')+String(ll_lotefc,'0000')
					
					dw_3.SetItem(ll_fila,"foliolote",ls_lote)
					dw_3.Object.nuevo[ll_fila] = 1
					
				NEXT	
				
				pb_imprimir.Enabled	= True
				pb_eliminar.Enabled  = NOT istr_mant.Solo_Consulta
				pb_grabar.Enabled		= NOT istr_mant.Solo_Consulta
				pb_ins_det.Enabled	= NOT istr_mant.Solo_Consulta
				pb_eli_det.Enabled	= NOT istr_mant.Solo_Consulta
				il_filadw3 = 1
				dw_3.SetRow(1)
				dw_3.SelectRow(1,false)
				
				IF dw_3.RowCount()>0 THEN
					dw_4.SetRedraw(FALSE)
					dw_4.SetFilter("plde_codigo = " + String(dw_3.Object.plde_codigo[1]) + " AND " + &
   	            				"lote_espcod = " + String(dw_3.Object.lote_espcod[1]) + " AND " + &
										"lote_codigo = " + String(dw_3.Object.lote_codigo[1]))
					dw_4.Filter()
					dw_4.SetRedraw(TRUE)
				END IF
				
				dw_3.SetFocus()
				
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo_detalle();Integer	li_tabpage
String   ls_lote
Boolean	lb_estado

li_tabpage			=	tab_1.SelectedTab

CHOOSE CASE li_tabpage
	CASE 1
		
		IF dw_3.RowCount() < 7 THEN
			il_filadw3=dw_3.insertrow(0)
					 
			dw_3.Object.plde_codigo[il_filadw3] = dw_2.Object.plde_codigo[1]
			dw_3.Object.lote_espcod[il_filadw3] = dw_2.Object.espe_codigo[1]
			
			dw_3.Object.nuevo[il_filadw3] = 0
			
			IF il_filadw3 > 0 THEN
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eli_det.Enabled	= True
			END IF
			dw_3.ScrollToRow(il_filadw3)
			dw_3.SetRow(il_filadw3)
			dw_3.SetFocus()
			dw_3.SetColumn(1)
		ELSE
			Messagebox("Atención","No se Pueden Ingresar más de Seis Lotes")
		END IF	

	CASE 2
		il_filadw6=dw_6.insertrow(0)
		IF il_filadw6 > 0 THEN
			pb_eliminar.Enabled	= True
			pb_grabar.Enabled		= True
	      pb_eli_det.Enabled	= True
		END IF

		dw_6.ScrollToRow(il_filadw6)
		dw_6.SetRow(il_filadw6)
		dw_6.SetFocus()
		dw_6.SetColumn(1)
		
END CHOOSE


end event

event closequery;

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
					CASE 3
						Message.ReturnValue = 1
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF
//
end event

event ue_modifica_detalle();//
end event

event ue_antesguardar();Long ll_fila, ll_lotefc
Integer li_planta, li_especie, li_secuen
String  ls_fecha, ls_hora

dw_3.accepttext()
dw_6.Accepttext()

ll_fila=1

Do While ll_fila<=dw_3.RowCount()
	IF isnull(dw_3.Object.foliolote[ll_fila]) OR dw_3.Object.foliolote[ll_fila] = "" THEN
		dw_3.DeleteRow(ll_Fila)
	ELSE
		ll_fila++
	END IF	
Loop	

FOR ll_fila = 1 TO dw_4.RowCount()
	IF dw_4.Object.ccde_podade[ll_fila] > 99.99  THEN
		Messagebox("Atención","La Fila " + string(ll_fila) + " en Daños por Lotes Posee un Porcentaje Mayor o igual al 100 % .")
		Message.DoubleParm = -1
		Return 
	 END IF
	 
NEXT	

FOR ll_fila = 1 TO dw_6.RowCount()
	IF dw_6.Object.cccm_firmez[ll_fila] > 99.99 OR dw_6.Object.cccm_coracl[ll_fila] > 99.99 OR & 
	   dw_6.Object.cccm_coracm[ll_fila] > 99.99 OR dw_6.Object.cccm_coracs[ll_fila] > 99.99 OR &
		dw_6.Object.cccm_pardea[ll_fila] > 99.99 OR dw_6.Object.cccm_segreg[ll_fila] > 99.99 THEN
		Messagebox("Atención","La Fila " + string(ll_fila) + " en Madurez Posee un Porcentaje Mayor o igual al 100 % .")
		Message.DoubleParm = -1
		Return 
	 END IF
	 
NEXT

FOR ll_fila = 1 TO dw_3.RowCount()
	IF dw_3.Object.ccfe_fruemb[ll_fila] > 999999  THEN
		Messagebox("Atención","La Fila " + string(ll_fila) + " Posee un valor de Fruta Embalada superior al Permitido")
		Message.DoubleParm = -1
		Return 
	 END IF
	 
NEXT	

dw_4.SetRedraw(FALSE)
dw_4.Accepttext()

IF dw_3.RowCount() > 0 THEN
	li_planta 	= dw_3.Object.plde_codigo[il_filadw3]
	li_especie 	= dw_3.Object.lote_espcod[il_filadw3]
	ll_lotefc 	= dw_3.Object.lote_codigo[il_filadw3]
	
	dw_4.SetFilter("")
	dw_4.Filter()
	FOR ll_fila = 1 TO dw_4.Rowcount()
		IF dw_4.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
			dw_4.SetItem(ll_fila,"ccco_folio",dw_2.Object.ccco_folio[1])
		END IF
	NEXT	
	dw_4.SetFilter("plde_codigo = " + String(li_planta) + " AND " + &
   	            "lote_espcod = " + String(li_especie) + " AND " + &
						"lote_codigo = " + String(ll_lotefc))
	dw_4.Filter()
END IF

dw_4.SetRedraw(TRUE)


ls_fecha = Mid(String(dw_2.Object.ccco_fecha[1]),1,10)

FOR ll_fila = 1 TO dw_3.Rowcount()
	IF dw_3.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
 		dw_3.SetItem(ll_fila,"ccco_folio",dw_2.Object.ccco_folio[1])
		 
      ls_hora = Mid(String(dw_3.Object.ccfe_horrev[ll_fila]),12,5)
		dw_3.Object.ccfe_horrev[ll_fila] = Time(ls_hora)
	END IF
NEXT

FOR ll_fila = 1 TO dw_6.Rowcount()
	IF dw_6.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_6.SetItem(ll_fila,"plde_codigo",dw_2.Object.plde_codigo[1])
		dw_6.SetItem(ll_fila,"ccco_folio",dw_2.Object.ccco_folio[1])
	END IF
NEXT


dw_2.Object.ccco_totbul[1] =  Round(dw_3.Object.total_bulto[dw_3.getrow()],2)


end event

event ue_borra_detalle();Integer	li_tabpage, li_borra
Long  ll_fila
Boolean	lb_estado

li_tabpage	=	tab_1.SelectedTab

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

CHOOSE CASE li_tabpage
	CASE 1
		IF dw_3.RowCount() < 1 THEN
		   Message.DoubleParm = -1
		ELSE	
			li_borra	=	dw_3.DeleteRow(il_filadw3)
			IF li_Borra = 1 THEN
				li_Borra = dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
				
				ll_fila=dw_3.GetRow()
				
				dw_4.SetRedraw(FALSE)
				
				dw_4.SetFilter("plde_codigo = " + String(dw_3.Object.plde_codigo[ll_fila]) + " AND " + &
               				"lote_espcod = " + String(dw_3.Object.lote_espcod[ll_fila]) + " AND " + &
									"lote_codigo = " + String(dw_3.Object.lote_codigo[ll_fila]))
				dw_4.Filter()
				dw_4.SetRedraw(TRUE)

			END IF
		END IF	
			
	CASE 2
		IF dw_6.RowCount() < 1 THEN
			Message.DoubleParm = -1
		ELSE
			li_borra	=	dw_6.DeleteRow(0)
		END IF	
		
END CHOOSE
 
IF Message.DoubleParm = -1 THEN RETURN

IF li_borra = 1 THEN
		
	ib_borrar = False
	w_main.SetMicroHelp("Borrando Registro...")
	SetPointer(Arrow!)
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF

IF dw_3.RowCount() = 0 and dw_6.RowCount() = 0  THEN pb_eli_det.Enabled = False

istr_mant.borra	 = False

end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)

dw_4.SetRedraw(FALSE)
dw_4.SetFilter("")
dw_4.Filter()
dw_4.SetRedraw(TRUE)

IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)

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

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
Integer li_variedad, li_especie

istr_info.titulo	= "CONTROL DE CALIDAD FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_concalcom_composite"

vinf.dw_1.SetTransObject(sqlca)

li_especie  = dw_2.Object.espe_codigo[1]
li_variedad = dw_2.Object.vari_codigo[1]

SELECT vari_codigo into :li_variedad
  FROM dba.spro_danosydefectos
 WHERE espe_codigo = :li_especie
   AND vari_codigo = :li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Control Calidad Comercial")
	setnull(li_variedad)
ELSEIF sqlca.SQLCode = 100 THEN
	setnull(li_variedad)
END IF	

fila = vinf.dw_1.Retrieve(dw_2.Object.plde_codigo[1],dw_2.Object.ccco_folio[1],li_variedad)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_seleccion();call super::ue_seleccion;String ls_lote
Long   ll_fila
Str_busqueda	lstr_busq

lstr_busq.argum[1] = String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_control_calidad_comercial, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("ccco_folio")
	dw_2.SetFocus()
ELSE
	dw_2.Object.plde_codigo[1]	=	Integer(lstr_busq.argum[1])
	dw_2.Object.ccco_folio[1]	=	Long(lstr_busq.argum[2])
	istr_mant.argumento[1]	=	lstr_busq.argum[1]
	istr_mant.argumento[2]	=	lstr_busq.argum[2]
   TriggerEvent("ue_recuperadatos")
END IF	
	
end event

event ue_listo();IF dw_3.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_concalcom
boolean visible = false
integer x = 37
integer y = 1700
integer width = 3154
integer height = 176
boolean hscrollbar = false
boolean vscrollbar = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_concalcom
integer x = 46
integer width = 3136
integer height = 660
string dataobject = "dw_mant_concalcomenca"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_Nula,	ls_Columna
Date		ld_Fecha

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "plde_codigo"
		
		IF Not iuo_planta.existe(integer(data), True, SQLCA) THEN
			this.SetItem(1,ls_Columna,integer(ls_Nula))
			Return 1
		ELSE	
			istr_mant.Argumento[1] = Data
		END IF	
		
	CASE "ccco_folio"
		
		istr_mant.Argumento[2] = Data
		IF Existefolio(Long(data)) THEN
		   parent.triggerevent("ue_recuperadatos")	
			Return 1
		END IF
    
	CASE "ccco_fecha"
		ld_Fecha	=	Date(Mid(data,1,10))
		
//		IF NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) THEN
//			This.SetItem(1,"mfco_fecmov",Date(ls_Nula))
//			This.SetFocus()
//			RETURN 1
//		END IF

	CASE "ccco_tipdoc"
		istr_mant.argumento[3] = Data
		this.setitem(1,"ccco_docrel",long(ls_Nula))
		istr_mant.argumento[4] = ""
		This.SetItem(1,"espe_codigo", Integer(ls_Nula))
		This.SetItem(1,"vari_codigo", Integer(ls_Nula))
		This.SetItem(1,"prod_codigo", long(ls_Nula))
		
   CASE "ccco_docrel"
		
		istr_mant.argumento[4] = ""
		IF Not existeorden(dw_2.Object.ccco_tipdoc[1],long(data)) THEN
			This.SetItem(1, ls_Columna, long(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.argumento[4] = Data
		END IF
		
	CASE "espe_codigo"
		IF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			dw_2.GetChild("vari_codigo",idwc_variedad)
			idwc_Variedad.SetTransObject(SQLCA)
			IF idwc_Variedad.Retrieve(gstr_parempresa.empr_codexp,Integer(data)) = 0 THEN
				idwc_variedad.Insertrow(0)
			END IF	
		END IF
    
	CASE "prod_codigo"
		IF Not iuo_productor.existe(Long(data),True,SQLCA) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF	
	 
	 CASE "line_codigo"
		IF Not iuo_linea.existe(integer(istr_mant.Argumento[1]),Integer(data),True,SQLCA) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
	CASE "vari_codigo"
		IF Not iuo_variedad.existe(dw_2.Object.espe_codigo[1],Integer(data),True,SQLCA) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF	
		
END CHOOSE

HabilitaIngreso(ls_Columna)

end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "b_orden"
		buscaorden()

		
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_concalcom
integer x = 3305
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_concalcom
integer x = 3305
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_concalcom
integer x = 3305
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_concalcom
integer x = 3305
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_concalcom
integer x = 3305
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_concalcom
integer x = 3282
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_concalcom
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_concalcom
integer x = 3305
end type

type tab_1 from tab within w_maed_spro_concalcom
integer x = 41
integer y = 756
integer width = 3154
integer height = 1064
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 30586022
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_3 tp_3
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_3=create tp_3
this.Control[]={this.tp_1,&
this.tp_3}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_3)
end on

type tp_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3118
integer height = 936
long backcolor = 30586022
string text = "Daños por Lote"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Custom066!"
long picturemaskcolor = 536870912
dw_daños dw_daños
dw_lotes dw_lotes
end type

on tp_1.create
this.dw_daños=create dw_daños
this.dw_lotes=create dw_lotes
this.Control[]={this.dw_daños,&
this.dw_lotes}
end on

on tp_1.destroy
destroy(this.dw_daños)
destroy(this.dw_lotes)
end on

type dw_daños from datawindow within tp_1
integer x = 1435
integer y = 84
integer width = 1669
integer height = 788
integer taborder = 30
string title = "none"
string dataobject = "dw_mant_mues_spro_concalcomdeta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_filadw4 = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_filadw4,False)
	
END IF

RETURN 0
end event

type dw_lotes from datawindow within tp_1
event dwnkey pbm_dwnkey
integer x = 50
integer y = 84
integer width = 1385
integer height = 788
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_daños_por_lotes"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event type long dwnkey(keycode key, unsignedlong keyflags);il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		Parent.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		Parent.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event buttonclicked;CHOOSE CASE dwo.Name

	CASE "b_lote"
		buscalote()

		
END CHOOSE
end event

event clicked;IF Row > 0 THEN
	
	il_filadw3 = Row
	This.SelectRow(0,False)
	This.SetRow(il_filadw3)
	accepttext()	
	IF isnull(dw_3.Object.foliolote[il_filadw3]) = False THEN
		dw_4.SetRedraw(FALSE)
		dw_4.SetFilter("plde_codigo = " + String(dw_3.Object.plde_codigo[il_filadw3]) + " AND " + &
   		            "lote_espcod = " + String(dw_3.Object.lote_espcod[il_filadw3]) + " AND " + &
							"lote_codigo = " + String(dw_3.Object.lote_codigo[il_filadw3]))
		dw_4.Filter()
		dw_4.SetRedraw(TRUE)
	END IF	
	
END IF

RETURN 0

end event

event losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event itemchanged;Long     ll_fila, ll_lote
Integer  li_secuen
String	ls_Nula,	ls_Columna, ls_lote, ls_especie, ls_planta

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "foliolote"
		IF len(data) = 10 THEN
			ls_planta	=	Mid(data,1,4)
		  	IF Integer(ls_planta)=dw_2.Object.plde_codigo[1] THEN
				ls_especie	=	Mid(data,5,2)
				IF Integer(ls_especie)=dw_2.Object.espe_codigo[1] THEN
					ls_lote 	=	Mid(data,7,4)
					IF Existelote(long(ls_lote)) THEN
						IF NOT Duplicadolote(data) THEN
							dw_3.Object.lote_codigo[il_filadw3] = integer(ls_lote)
							IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
								Buscadanos(dw_3.Object.plde_codigo[il_filadw3],dw_3.Object.lote_espcod[il_filadw3], &
										  long(ls_lote))
							ELSE			  
								FOR ll_fila=1 TO dw_4.RowCount()
									dw_4.Object.lote_codigo[ll_fila] = long(ls_lote)
								NEXT	
							END IF
						ELSE
							IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
								THIS.SetItem(il_filadw3,ls_columna,ls_nula)
								Return 1
							ELSE
								THIS.SetItem(il_filadw3,ls_columna,this.Object.foliolote[il_filadw3])
								Return 1
							END IF
						END IF
					ELSE
						IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
							THIS.SetItem(il_filadw3,ls_columna,ls_nula)
							Return 1
						ELSE
							THIS.SetItem(il_filadw3,ls_columna,this.Object.foliolote[il_filadw3])
							Return 1
						END IF
					END IF
				ELSE
					MessageBox("Error de Consistencia","El Folio de Lote No Corresponde a la Especie Seleccionada")
					IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
						THIS.SetItem(il_filadw3,ls_columna,ls_nula)
						Return 1
					ELSE
						THIS.SetItem(il_filadw3,ls_columna,this.Object.foliolote[il_filadw3])
						Return 1
					END IF
				END IF					
			ELSE
				MessageBox("Error de Consistencia","El Folio de Lote No Corresponde a la Planta Seleccionada")
				IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
					THIS.SetItem(il_filadw3,ls_columna,ls_nula)
					Return 1
				ELSE
					THIS.SetItem(il_filadw3,ls_columna,this.Object.foliolote[il_filadw3])
					Return 1
				END IF
			END IF			
		ELSE
			messagebox("Error de Consistencia","Ingrese un largo de diez dígitos")
			IF dw_3.Object.nuevo[il_filadw3] = 0 THEN
				THIS.SetItem(il_filadw3,ls_columna,ls_nula)
					Return 1
				ELSE
					THIS.SetItem(il_filadw3,ls_columna,this.Object.foliolote[il_filadw3])
					Return 1
				END IF
		END IF	
	
END CHOOSE		
end event

event itemerror;String	s

s	=	This.Describe(This.GetColumnName()+".coltype")

CHOOSE CASE s
	CASE "number"
		IF Trim(This.GetText())= "" OR Not IsNumber(Trim(This.GetText())) THEN
			Int	null_num
			
			SetNull(null_num)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_num)
			
			RETURN 3
		END IF
		
	CASE "date"
		IF Trim(This.GetText()) = "" THEN
			Date	null_date
			
			SetNull(null_date)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_date)
			
			RETURN 3
		END IF
		
	CASE "time"
		IF Trim(This.GetText()) = "" THEN
			Time	null_time
			
			SetNull(null_time)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_time)
			
			RETURN 3
		END IF
		
	CASE "datetime"
		IF Trim(This.GetText()) = "" THEN
			Date	null_datetime
			
			SetNull(null_datetime)
			
			This.SetItem(This.GetRow(),This.GetColumn(),null_datetime)
			
			RETURN 3
		END IF
		
	CASE ELSE
		RETURN 1
		
END CHOOSE
end event

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

event itemfocuschanged;IF Row > 0 THEN
	
	il_filadw3 = Row
	This.SelectRow(0,False)
	This.SetRow(il_filadw3)
	
	accepttext()
	
	IF isnull(dw_3.Object.foliolote[il_filadw3]) = False and ib_borrar=false THEN
		dw_4.SetRedraw(FALSE)
		dw_4.SetFilter("plde_codigo = " + String(dw_3.Object.plde_codigo[il_filadw3]) + " AND " + &
   		            "lote_espcod = " + String(dw_3.Object.lote_espcod[il_filadw3]) + " AND " + &
							"lote_codigo = " + String(dw_3.Object.lote_codigo[il_filadw3]))
		dw_4.Filter()
		dw_4.SetRedraw(TRUE)
	END IF	
	
END IF

IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_filadw3 = getrow()
	This.SelectRow(0,False)
		
	IF isnull(dw_3.Object.foliolote[il_filadw3]) = False THEN
		dw_4.SetRedraw(FALSE)
		dw_4.SetFilter("plde_codigo = " + String(dw_3.Object.plde_codigo[il_filadw3]) + " AND " + &
   		            "lote_espcod = " + String(dw_3.Object.lote_espcod[il_filadw3]) + " AND " + &
							"lote_codigo = " + String(dw_3.Object.lote_codigo[il_filadw3]))
		dw_4.Filter()
		dw_4.SetRedraw(TRUE)
	END IF	
	
END IF

RETURN 0
end event

type tp_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3118
integer height = 936
long backcolor = 12632256
string text = "Madurez"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Environment!"
long picturemaskcolor = 536870912
dw_madurez dw_madurez
end type

on tp_3.create
this.dw_madurez=create dw_madurez
this.Control[]={this.dw_madurez}
end on

on tp_3.destroy
destroy(this.dw_madurez)
end on

type dw_madurez from datawindow within tp_3
integer x = 59
integer y = 64
integer width = 2981
integer height = 820
integer taborder = 30
string dataobject = "dw_mant_mues_spro_concalcomdetmad"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Nula,	ls_Columna

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "cccm_gcalib"
		
		IF DuplicadoCalibre(data) THEN
			this.setitem(row,ls_columna,ls_nula)
			return 1
		END IF	
		
END CHOOSE		
end event

event clicked;IF Row > 0 THEN
	
	il_filadw6 = Row
	This.SelectRow(0,False)
	This.SetRow(il_filadw6)
	
END IF

RETURN 0
end event

event itemerror;Return 1
end event

event rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_filadw6 = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_filadw6,False)
	
END IF

RETURN 0
end event

