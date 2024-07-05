import FS from 'fs/promises'
import Spawn from 'child_process'
import Path from 'path'

export const gitignoringGlobTimeTravel = promise2aff => commit => a => b => promise2aff(async () => {
  const exist = a => FS.stat(a).then(() => true).catch(() => false) 
  if (!await exist('.tmp')) {
    await FS.mkdir('.tmp')
  }

  const p = `.tmp/spago-${commit.slice(0, 7)}`

  if (!await exist(p)) {
    Spawn.execSync(`git worktree add -f ${p} ${commit}`, {stdio: [null, 'inherit', 'inherit']})
    Spawn.execSync(`cp -R output ${p}/output`, {stdio: [null, 'inherit', 'inherit']})
    Spawn.execSync(`cp -R .spago ${p}/.spago`, {stdio: [null, 'inherit', 'inherit']})
    Spawn.execSync(`cd ${p} && spago build`, {stdio: [null, 'inherit', 'inherit']})
  }

  const {gitignoringGlob} = await import(Path.resolve(process.cwd(), `${p}/output/Spago.Glob/index.js`))
  const {fromAff: aff2promise} = await import(Path.resolve(process.cwd(), `${p}/output/Control.Promise/index.js`))
  return promise2aff(() => aff2promise(gitignoringGlob(a)(b))())
})
