$PBExportHeader$w_maed_spro_ordenretiroventaenca.srw
forward
global type w_maed_spro_ordenretiroventaenca from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_spro_ordenretiroventaenca
end type
end forward

global type w_maed_spro_ordenretiroventaenca from w_mant_encab_deta_csd
string title = "Orden de Retiro"
string menuname = ""
boolean controlmenu = false
dw_3 dw_3
end type
global w_maed_spro_ordenretiroventaenca w_maed_spro_ordenretiroventaenca

type variables
w_mant_deta_spro_ordenventacomdeta w_mantencion

uo_plantadesp		iuo_planta

datawindowchild	idwc_planta, idwc_especie, idwc_categoria, idwc_frio, idwc_vende

Boolean				ib_Modifica, ib_AutoCommit
String is_rut


end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existemovimiento (long al_numero)
public function integer buscaproductor ()
public subroutine buscaordenventa ()
public function boolean existeordenventa (long al_numero)
public function boolean prodrechazado (string as_rut)
public subroutine habilitaingreso ()
public function decimal buscabultoexis ()
end prototypes

public subroutine habilitaencab (boolean habilita);

IF Habilita THEN
	
	dw_2.Object.oret_numero.Protect 				=	0
	dw_2.Object.oret_numero.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.oret_fecret.Protect 				=	0
	dw_2.Object.oret_fecret.BackGround.Color 	=	RGB(255,255,255)
	dw_2.Object.odfc_numero.Protect 				=	0
	dw_2.Object.odfc_numero.BackGround.Color 	=	RGB(255,255,255)
	
ELSE
	
	dw_2.Object.oret_numero.Protect 				=	1
	dw_2.Object.oret_numero.BackGround.Color 	=	RGB(166,180,210)
	dw_2.Object.oret_fecret.Protect 				=	1
	dw_2.Object.oret_fecret.BackGround.Color 	=	RGB(166,180,210)
	dw_2.Object.odfc_numero.Protect 				=	1
	dw_2.Object.odfc_numero.BackGround.Color 	=	RGB(166,180,210)
	
END IF	
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = -1 THEN
		IF dw_2.Update(True, False) = -1 THEN
			Commit;
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			ELSE
				lb_Retorno	=	True
		
				dw_2.ResetUpdate()
				dw_1.ResetUpdate()		
			END IF	
		ELSE
			RollBack;
			F_ErrorBaseDatos(sqlca, This.Title)
   	END IF
	ELSE
		RollBack;
		F_ErrorBaseDatos(sqlca, This.Title)
   END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN		 				 //Encabezado Retiro
		IF dw_1.Update(True, False) = 1 THEN					 //Detalle Retiro
			Commit;
				
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			ELSE
				lb_Retorno	=	True
					
			   dw_2.ResetUpdate()
				dw_1.ResetUpdate()
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

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public function boolean existemovimiento (long al_numero);Integer	li_tipodoc, li_planta
Long     ll_nrodocto

li_Planta  = dw_2.Object.plde_codigo[1]
li_Tipodoc = integer(istr_mant.argumento[2])

SELECT	oret_numero
	INTO	:ll_nrodocto
	FROM	dba.spro_ordenretiroventaenc
	WHERE	plde_codigo	=	:li_Planta
	AND   tdop_codigo =  :li_TipoDoc
	AND	oret_numero	=	:al_Numero;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Orden de Retiro Encabezado")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

RETURN True
end function

public function integer buscaproductor ();Long	  ll_productor
String  ls_rut

ls_rut = dw_2.Object.clpr_rut[1]

SELECT	prod_codigo
	INTO	:ll_productor
	FROM	dba.clienprove
	WHERE	clpr_rut		=	:ls_rut;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Orden de Retiro Encabezado")

	RETURN 0
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN 0
END IF

RETURN ll_productor
end function

public subroutine buscaordenventa ();Str_Busqueda	lstr_busq
Long   ll_Fila, ll_row

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = ''

OpenWithParm(w_busc_ordenventacomercial, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	
	IF Not prodrechazado(lstr_busq.argum[3]) THEN
	
		istr_mant.argumento[4]	=	lstr_busq.argum[2]
		
		dw_2.SetItem(1,"odfc_numero", Long(lstr_busq.argum[2]))
		dw_2.SetItem(1,"clpr_rut"   , lstr_busq.argum[3])
		dw_2.SetItem(1,"clpr_nombre", lstr_busq.argum[4])
		dw_2.SetItem(1,"odfc_tipret", Integer(lstr_busq.argum[5]))
		dw_2.SetItem(1,"odfc_observ", lstr_busq.argum[6])
		
	
		
		dw_3.Retrieve(integer(lstr_busq.argum[1]),Long(lstr_busq.argum[2]))
		
		IF Integer(lstr_busq.argum[5]) = 2 THEN
			FOR ll_row=1 TO dw_3.RowCount()
			
				ll_Fila = dw_1.InsertRow(0)
				
				dw_1.Object.plde_codigo[ll_fila] = dw_3.Object.plde_codigo[ll_Row]
				dw_1.Object.espe_codigo[ll_fila] = dw_3.Object.espe_codigo[ll_Row]
				dw_1.Object.grva_codigo[ll_fila] = dw_3.Object.grva_codigo[ll_Row]
				dw_1.Object.grva_nombre[ll_fila] = dw_3.Object.grva_nombre_grupo[ll_Row]
				dw_1.Object.grva_codsub[ll_fila] = dw_3.Object.grva_codsub[ll_Row]
				dw_1.Object.grva_nombre_1[ll_fila] = dw_3.Object.grva_nombre_subgrupo[ll_Row]
				dw_1.Object.vari_codigo[ll_fila] = dw_3.Object.vari_codigo[ll_Row]
				dw_1.Object.vari_nombre[ll_fila] = dw_3.Object.vari_nombre[ll_Row]
				dw_1.Object.cate_codigo[ll_fila] = dw_3.Object.cate_codigo[ll_Row]
				dw_1.Object.frio_tipofr[ll_fila] = dw_3.Object.frio_tipofr[ll_Row]
				dw_1.Object.enva_tipoen[ll_fila] = dw_3.Object.enva_tipoen[ll_Row]
				dw_1.Object.enva_codigo[ll_fila] = dw_3.Object.enva_codigo[ll_Row]
				dw_1.Object.odre_gcalib[ll_fila] = dw_3.Object.ofcd_gcalib[ll_Row]
				dw_1.Object.odre_bultos[ll_fila] = 0
				
			NEXT
			dw_1.Object.odre_bultos.Protect = 0
			pb_grabar.Enabled = TRUE
		
		ELSE
			dw_1.Object.odre_bultos.Protect = 1
			pb_grabar.Enabled = TRUE
		
		END IF	
	END IF
END IF
end subroutine

public function boolean existeordenventa (long al_numero);Integer	li_tipodoc, li_planta, li_tipret, li_pesfac
Long     ll_nrodocto, ll_Fila, ll_Row
String   ls_rut, ls_observ, ls_nombrecl

li_Planta  = dw_2.Object.plde_codigo[1]

SELECT	ov.odfc_numero, ov.clpr_rut, ov.odfc_tipret, ov.odfc_observ, cl.clpr_nombre,
         ov.odfc_pesfac
	INTO	:ll_nrodocto, :ls_rut, :li_tipret, :ls_observ, :ls_nombrecl, :li_pesfac
	FROM	dba.spro_ordenventacomenca ov, dba.clienprove cl
	WHERE	ov.plde_codigo	=	:li_Planta
	AND	ov.odfc_numero	=	:al_Numero
	AND   ov.clpr_rut		=	cl.clpr_rut;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Orden de Venta Encabezado")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

istr_mant.argumento[4]	=	String(ll_nrodocto)
	
dw_2.SetItem(1,"odfc_numero", ll_nrodocto)
dw_2.SetItem(1,"clpr_rut"   , ls_rut)
dw_2.SetItem(1,"clpr_nombre", ls_nombrecl)
dw_2.SetItem(1,"odfc_tipret", li_tipret)
dw_2.SetItem(1,"odfc_observ", ls_observ)
	
dw_3.Retrieve(li_planta,ll_nrodocto)
	
IF li_tipret = 2 THEN
	FOR ll_row=1 TO dw_3.RowCount()
	
		ll_Fila = dw_1.InsertRow(0)
			
		dw_1.Object.plde_codigo[ll_fila] = dw_3.Object.plde_codigo[ll_Row]
		dw_1.Object.espe_codigo[ll_fila] = dw_3.Object.espe_codigo[ll_Row]
		dw_1.Object.grva_codigo[ll_fila] = dw_3.Object.grva_codigo[ll_Row]
		dw_1.Object.grva_nombre[ll_fila] = dw_3.Object.grva_nombre_grupo[ll_Row]
		dw_1.Object.grva_codsub[ll_fila] = dw_3.Object.grva_codsub[ll_Row]
		dw_1.Object.grva_nombre_1[ll_fila] = dw_3.Object.grva_nombre_subgrupo[ll_Row]
		dw_1.Object.vari_codigo[ll_fila] = dw_3.Object.vari_codigo[ll_Row]
		dw_1.Object.vari_nombre[ll_fila] = dw_3.Object.vari_nombre[ll_Row]
		dw_1.Object.cate_codigo[ll_fila] = dw_3.Object.cate_codigo[ll_Row]
		dw_1.Object.frio_tipofr[ll_fila] = dw_3.Object.frio_tipofr[ll_Row]
		dw_1.Object.enva_tipoen[ll_fila] = dw_3.Object.enva_tipoen[ll_Row]
		dw_1.Object.enva_codigo[ll_fila] = dw_3.Object.enva_codigo[ll_Row]
		dw_1.Object.odre_gcalib[ll_fila] = dw_3.Object.ofcd_gcalib[ll_Row]
		dw_1.Object.odre_bultos[ll_fila] = 0
		dw_1.Object.odre_tkilos[ll_fila] = 0
	NEXT
	
	IF li_pesfac = 1 THEN
		dw_1.Object.t_bultos.visible		= TRUE
		dw_1.Object.odre_bultos.visible 	= TRUE
		dw_1.Object.t_kilos.visible		= FALSE
		dw_1.Object.odre_tkilos.visible 	= FALSE
	ELSE
		dw_1.Object.t_bultos.visible		= FALSE
		dw_1.Object.odre_bultos.visible 	= FALSE
		dw_1.Object.t_kilos.visible		= TRUE
		dw_1.Object.odre_tkilos.visible 	= TRUE
	END IF
					
	dw_1.Object.odre_bultos.Protect = 0
	dw_1.Object.odre_tkilos.Protect = 0
	pb_grabar.Enabled = TRUE
	
ELSE
	
	IF li_pesfac = 1 THEN
		dw_1.Object.t_bultos.visible		= TRUE
		dw_1.Object.odre_bultos.visible 	= TRUE
		dw_1.Object.t_kilos.visible		= FALSE
		dw_1.Object.odre_tkilos.visible 	= FALSE
	ELSE
		dw_1.Object.t_bultos.visible		= FALSE
		dw_1.Object.odre_bultos.visible 	= FALSE
		dw_1.Object.t_kilos.visible		= TRUE
		dw_1.Object.odre_tkilos.visible 	= TRUE
	END IF
	
	dw_1.Object.odre_bultos.Protect = 1
	dw_1.Object.odre_tkilos.Protect = 1
	pb_grabar.Enabled = TRUE
	
END IF	

RETURN True
end function

public function boolean prodrechazado (string as_rut);String ls_nombre, ls_Null

SetNull(Ls_Null)

SELECT	pro.prod_nombre
	INTO	:ls_nombre
	FROM	dba.clienprove as cli, dba.productores pro
	WHERE	cli.clpr_nrorut	=	:as_rut
	And   pro.prod_codigo	=	cli.prod_codigo
	And   pro.prod_rechaz	=	1;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Proveedores")
	RETURN TRUE
ELSEIF SqlCa.SQLCode = 0 THEN
   dw_2.SetItem(1,"prod_rechaz",1)
	RETURN TRUE
END IF
RETURN TRUE
end function

public subroutine habilitaingreso ();Date			ld_fecha
Boolean		lb_estado = True

dw_2.AcceptText()

IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
	IsNull(dw_2.Object.oret_fecret[1]) OR dw_2.Object.oret_fecret[1] = ld_fecha OR &
	IsNull(dw_2.Object.odfc_numero[1]) OR dw_2.Object.odfc_numero[1] = 0 THEN
	lb_estado = False

END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public function decimal buscabultoexis ();Decimal{2} ld_bultos, ld_kilpro
Boolean	lb_AutoCommit
Integer  li_planta, li_especie, li_grupo, li_subgrupo, li_variedad, &
         li_categoria, li_tipoenva, li_envase
Long     ll_productor			
String   ls_calibre

li_planta  		=  dw_2.Object.plde_codigo[1]
ll_productor 	=	0 //Buscaproductor()
li_especie		=	dw_1.Object.espe_codigo[il_fila]
li_tipoenva		=  dw_1.Object.enva_tipoen[il_fila]
li_envase		=	dw_1.Object.enva_codigo[il_fila]

IF isnull(dw_1.Object.grva_codigo[il_fila]) THEN
	SetNull(li_grupo)
ELSE
	li_grupo =	dw_1.Object.grva_codigo[il_fila]
END IF

IF isnull(dw_1.Object.grva_codsub[il_fila]) THEN
	SetNull(li_subgrupo)
ELSE
	li_subgrupo =	dw_1.Object.grva_codsub[il_fila]
END IF

IF isnull(dw_1.Object.vari_codigo[il_fila]) THEN
	SetNull(li_variedad)
ELSE
	li_variedad =	dw_1.Object.vari_codigo[il_fila]
END IF

IF isnull(dw_1.Object.cate_codigo[il_fila]) THEN
	SetNull(li_categoria)
ELSE
	li_categoria =	dw_1.Object.cate_codigo[il_fila]
END IF

IF isnull(dw_1.Object.odre_gcalib[il_fila]) THEN
	SetNull(ls_calibre)
ELSE
	ls_calibre =	dw_1.Object.odre_gcalib[il_fila]
END IF

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	TRUE

DECLARE BuscaBultos PROCEDURE FOR dba.FComer_BuscaBultos_Existencia
    @Planta 		=	:li_Planta,
	 @Productor		=	:ll_Productor,
	 @Especie		=	:li_Especie,
	 @Grupo			=	:li_Grupo,
	 @SubGrupo		=	:li_SubGrupo,	
	 @Variedad		=	:li_Variedad,
	 @Categoria 	=	:li_Categoria,
	 @Tipoenvase	=	:li_Tipoenva,
	 @Envase			=	:li_Envase,
	 @GrupoCalibre	=	:ls_Calibre;
	EXECUTE BuscaBultos;
	
	FETCH BuscaBultos INTO :ld_Bultos, :ld_kilpro;
	
CLOSE BuscaBultos;

sqlca.AutoCommit	=	lb_AutoCommit

IF isnull(ld_Bultos) THEN ld_Bultos = 0
IF isnull(ld_kilpro) THEN ld_Kilpro = 0

IF dw_2.Object.odfc_pesfac[1] = 1 THEN
	RETURN ld_Bultos
ELSE
	RETURN ld_Kilpro
END IF	
end function

on w_maed_spro_ordenretiroventaenca.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_spro_ordenretiroventaenca.destroy
call super::destroy
destroy(this.dw_3)
end on

event open;/*
   	istr_mant.Argumento[1] = Planta
		istr_mant.Argumento[2] = 7 para orden de Retiro //sin uso
		istr_mant.Argumento[3] = Numero de Retiro
		istr_mant.Argumento[4] = Orden de Venta
		istr_mant.Argumento[5] = Fecha de Orden de Retiro
		
*/
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

istr_busq = Message.PowerObjectParm

//planta
dw_2.Getchild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve()

//Especie
dw_1.GetChild("espe_codigo",idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()

//Categoria
dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve()

//Vendedor
dw_2.GetChild("vend_codigo",idwc_vende)
idwc_vende.SetTransObject(SQLCA)
IF idwc_vende.Retrieve(0) = 0 THEN
	idwc_vende.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(SQLCA)
dw_3.SetTransObject(SQLCA)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								

buscar	= "Especie:Nespe_codigo,Variedad:Nvari_codigo"
ordenar	= "Especie:espe_codigo,Variedad:vari_codigo"
end event

event ue_nuevo();Long		ll_modif
String   ls_fecha

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 and ll_Modif > 0 THEN
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

HabilitaEncab(True)

ls_fecha = string(Today(),'dd/mm/yyyy')

istr_mant.Argumento[1] = string(gstr_paramplanta.codigoplanta)
istr_mant.Argumento[2] = "7"
istr_mant.Argumento[3] = ""
istr_mant.Argumento[4] = ""
istr_mant.Argumento[5] = ls_fecha

dw_2.SetItem(1,"plde_codigo",gstr_paramplanta.codigoplanta)
dw_2.SetItem(1,"oret_fecret",date(ls_fecha))
	
dw_2.SetColumn("oret_numero")
dw_2.SetFocus()

IF istr_busq.Argum[1] = "OV" THEN
	IF integer(istr_busq.argum[3]) = 1 THEN
		
		istr_mant.Argumento[3] = istr_busq.argum[4] 
		Triggerevent("ue_recuperadatos")
		
	ELSE
		
		Existeordenventa(long(istr_busq.argum[2]))
	
	END IF
	
	dw_1.SetColumn("odre_bultos")
	dw_1.SetFocus()
	
END IF	

end event

event ue_recuperadatos();Long		ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										  Integer(istr_mant.Argumento[2]), &
										  Long(istr_mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												  Integer(istr_mant.Argumento[2]), &
												  Long(istr_mant.Argumento[3]))
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF dw_2.RowCount()>0 THEN
					IF dw_2.Object.odfc_pesfac[1] = 1 THEN
						dw_1.Object.t_bultos.visible		= TRUE
						dw_1.Object.odre_bultos.visible 	= TRUE
						dw_1.Object.t_kilos.visible		= FALSE
						dw_1.Object.odre_tkilos.visible 	= FALSE
					ELSE
						dw_1.Object.t_bultos.visible		= FALSE
						dw_1.Object.odre_bultos.visible 	= FALSE
						dw_1.Object.t_kilos.visible		= TRUE
						dw_1.Object.odre_tkilos.visible 	= TRUE
					END IF	
						
					IF dw_2.Object.odfc_tipret[1] = 1 OR dw_2.Object.oret_estado[1]=2 THEN
						dw_1.Object.odre_bultos.Protect = 1
						dw_1.Object.odre_bultos.Protect = 1
					ELSE
						dw_1.Object.odre_bultos.Protect = 0
						dw_1.Object.odre_bultos.Protect = 0
					END IF	
				END IF

				pb_imprimir.Enabled	=	True
				pb_grabar.Enabled		=	True
				
				HabilitaEncab(False)
			END IF
			dw_2.SetRedraw(True)
		LOOP WHILE respuesta = 1
		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "ORDENES DE RETIRO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_spro_ordenretiroventa"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.Argumento[2]), long(istr_mant.argumento[3]))

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

event ue_antesguardar();Long		ll_Fila, ll_Secuen, ll_NumeroRetiro
Integer	li_Planta, li_TipoRetiro

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Planta			=	dw_2.Object.plde_codigo[1]
ll_NumeroRetiro	=	dw_2.Object.oret_numero[1]
li_TipoRetiro		= Integer(istr_mant.Argumento[2])

ll_Fila = 1


IF dw_1.RowCount()<=0 THEN 
	Message.DoubleParm = -1
ELSE	
		DO WHILE ll_Fila <= dw_1.RowCount()
			IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
				dw_1.DeleteRow(ll_Fila)
			ELSE
				ll_Fila ++
			END IF
		LOOP
		
		IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
				
				UPDATE	dba.spro_ordenretiroventaenc
					SET	oret_numero = 0
					WHERE	1 = 2;
				
				SELECT	IsNull(Max(oret_numero), 0) + 1
					INTO	:ll_NumeroRetiro
					FROM	dba.spro_ordenretiroventaenc
					WHERE	plde_codigo	=	:li_Planta
					  AND tdop_codigo =  :li_TipoRetiro;
					  
				dw_2.Object.tdop_codigo[1] = li_TipoRetiro
				dw_2.Object.oret_numero[1] = ll_NumeroRetiro
			
		END IF
		
		istr_Mant.Argumento[3]	=	String(dw_2.Object.oret_numero[1])
		
			UPDATE	dba.spro_ordenretiroventadet
				SET	oret_numero = 0
				WHERE	1 = 2;
			
			SELECT	IsNull(Max(odre_secuen), 0) + 1
				INTO	:ll_Secuen
				FROM	dba.spro_ordenretiroventadet
				WHERE	plde_codigo	=	:li_Planta
				  AND oret_numero =  :ll_NumeroRetiro;
			
		FOR ll_Fila = 1 TO dw_1.RowCount()
			IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN

				dw_1.Object.plde_codigo[ll_Fila]	=	li_Planta
				dw_1.Object.tdop_codigo[ll_Fila] =	li_TipoRetiro
				dw_1.Object.oret_numero[ll_Fila]	=	ll_NumeroRetiro
				dw_1.Object.odre_secuen[ll_Fila]	=	ll_secuen
				ll_secuen++
				
			END IF
		NEXT

END IF
end event

event ue_borra_detalle();IF dw_1.rowcount() < 2 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
 IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(w_mantencion, istr_mant)
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = ''

OpenWithParm(w_busc_ordenretiroventa, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.argumento[3]	=	lstr_busq.argum[2]
	
	This.TriggerEvent("ue_recuperadatos")
	
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_ordenretiroventaenca
integer x = 37
integer y = 728
integer width = 3141
integer height = 1012
string title = "Detalle de Orden de Venta"
string dataobject = "dw_mues_spro_ordenretiroventadeta"
end type

event dw_1::doubleclicked;//
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

RETURN 0
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, False)

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,False)
END IF

RETURN 0
end event

event dw_1::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, False)

RETURN 0
end event

event dw_1::itemchanged;call super::itemchanged;Long		ll_fila
String	ls_columna, ls_nula, ls_natural
Decimal {2} ld_bultos, ld_bultoex
SetNull(ls_Nula)
  
ls_columna = GetColumnName()

CHOOSE CASE ls_columna
		
	CASE "odre_bultos"
		
		ld_bultos  = Double(Data)
		ld_Bultoex = BuscaBultoexis()
		IF ld_bultos > ld_Bultoex THEN
			messagebox("Error de Cantidad","Los Bultos Ingresados son Mayores a la Existencia de esta Especie.")
			This.SetItem(il_fila,ls_columna,0)
			Return 1
		END IF
		
	CASE "odre_tkilos"
		
		ld_bultos  = Double(Data)
		ld_Bultoex = BuscaBultoexis()
		IF ld_bultos > ld_Bultoex THEN
			messagebox("Error de Cantidad","Los Kilos Ingresados son Mayores a la Existencia de esta Especie.")
			This.SetItem(il_fila,ls_columna,0)
			Return 1
		END IF
		
END  CHOOSE

end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_ordenretiroventaenca
integer x = 123
integer width = 2880
integer height = 576
string dataobject = "dw_mant_spro_ordenretiroenca"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Null, ls_Fecha

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna

	CASE "plde_codigo"
		
		IF Not iuo_planta.existe(integer(data),True,Sqlca) THEN
			This.SetItem(1, "plde_codigo", integer(ls_Null))
		ELSE
			istr_Mant.Argumento[1]	= Data
		END IF
	
	CASE "oret_numero"
		
		IF ExisteMovimiento(Long(Data)) THEN
			istr_Mant.Argumento[3]	=	Data
			Parent.TriggerEvent("ue_recuperadatos")
		ELSE
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF
	
	CASE "odfc_numero"
		
		IF ExisteOrdenVenta(Long(Data)) THEN
			istr_Mant.Argumento[4]	=	Data
			ProdRechazado(Istr_mant.Argumento[4])
		ELSE
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF

	CASE "oret_fecret"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, Date(ls_Fecha))
      istr_Mant.Argumento[5]=Mid(Data,1,10)
	
	
END CHOOSE

HabilitaIngreso()
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "b_orden"
    BuscaOrdenventa()		

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_ordenretiroventaenca
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_ordenretiroventaenca
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_ordenretiroventaenca
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_ordenretiroventaenca
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_ordenretiroventaenca
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_ordenretiroventaenca
boolean visible = false
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_ordenretiroventaenca
boolean visible = false
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_ordenretiroventaenca
end type

type dw_3 from datawindow within w_maed_spro_ordenretiroventaenca
boolean visible = false
integer x = 91
integer y = 1172
integer width = 3026
integer height = 532
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_ordenventacomdeta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

